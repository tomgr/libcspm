{-# LANGUAGE BangPatterns #-}
module CSPM.Evaluator.Profiler (
    ProfilerState, initialProfilerState,
    maybeRegisterCall, registerCall,
    ProfilingData, getProfilingData,
    profilerActive,
) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import qualified Data.HashTable.IO as H
import Data.List ((\\), isPrefixOf, sort, sortBy, transpose)
import System.IO.Unsafe

import CSPM.DataStructures.Names
import CSPM.Evaluator.Monad
import Util.PrettyPrint

type CallCountTable = H.BasicHashTable Name Int
data ProfilerState = ProfilerState {
        isActive :: Bool,
        tableLock :: MVar (),
        callCounts :: CallCountTable,
        hiearchicalCallCounts :: H.BasicHashTable [Name] CallCountTable,
        profilingStack :: [Name]
    }

profilerActive :: EvaluationState -> Bool
profilerActive st = isActive (profilerState st)

initialProfilerState :: Bool -> IO ProfilerState
initialProfilerState isActive = do
    callCounts <- H.new
    hiearchicalCallCounts <- H.new
    tableLock <- newMVar ()
    return $! ProfilerState {
        isActive = isActive,
        tableLock = tableLock,
        callCounts = callCounts,
        hiearchicalCallCounts = hiearchicalCallCounts,
        profilingStack = []
    }

maybeRegisterCall ::
    EvaluationMonad (Name -> EvaluationMonad a -> EvaluationMonad a)
maybeRegisterCall = do
    profilerState <- gets profilerState
    return $! if isActive profilerState then registerCall else \ _ prog -> prog

registerCall :: Name -> EvaluationMonad a -> EvaluationMonad a
registerCall n prog = do
    profilerState <- gets profilerState
    unsafePerformIO (incrementCounter profilerState n) `seq`
        modify (\ st -> st {
            profilerState = profilerState {
                profilingStack = n : profilingStack profilerState
            }
        }) prog
{-# NOINLINE registerCall #-}

incrementCallCounter :: CallCountTable -> Name -> IO ()
incrementCallCounter table n = do
    mcount <- H.lookup table n
    let !c = case mcount of
                Just count -> count + 1
                Nothing -> 1
    H.insert table n c

incrementCounter :: ProfilerState -> Name -> IO ()
incrementCounter profilerState n = do
    let lock = tableLock profilerState
        hierachicalTable = hiearchicalCallCounts profilerState
    bracket (takeMVar lock) (\_ -> putMVar lock ()) $ \_ -> do
        incrementCallCounter (callCounts profilerState) n

        let stk = profilingStack profilerState
        mtable <- H.lookup hierachicalTable stk
        table <- case mtable of
                    Just table -> return table
                    Nothing -> do
                        table <- H.new
                        H.insert hierachicalTable stk table
                        return table
        incrementCallCounter table n
{-# NOINLINE incrementCounter #-}

type CallCounts = [(Name, Int)]
data HierarchicalCallCount =
    HierarchicalCallCount [(Name, Int, HierarchicalCallCount)]
    deriving Show

createHierarchicalCallCount :: [([Name], CallCounts)] -> HierarchicalCallCount
createHierarchicalCallCount table =
    let reversedTable = map (\ (ns, c) -> (reverse ns, c)) table
        -- Sort the table lexiographically. This will cluster together
        -- common prefixes
        sortedTable = sortBy (\ (ns1, _) (ns2, _) -> compare ns1 ns2) reversedTable

        extractCommonPrefix :: [([Name], CallCounts)] ->
            (Name, [([Name], CallCounts)], [([Name], CallCounts)])
        extractCommonPrefix [] = error "invalid list"
        extractCommonPrefix (([], cs) : _) = error "invalid list"
        extractCommonPrefix ((n:ns1, cs1) : ncs) =
            let
                (common, rest) = span (\ (ns2, _) -> [n] `isPrefixOf` ns2) ncs
            in (n, (ns1, cs1) : (map (\ (ns, cs) -> (tail ns, cs)) common), rest)

        construct :: [([Name], CallCounts)] -> [(Name, Int, HierarchicalCallCount)]
        construct ncs =
            let
                -- How many time each top-level function was called
                counts = [(n, c) | ([], ncs') <- ncs, (n, c) <- ncs']

                recursiveCounts = [(xs, ncs') | (xs, ncs') <- ncs, xs /= []]

                -- Group into common prefixes
                commonPrefixes :: [([Name], CallCounts)] ->
                    [(Name, [([Name], CallCounts)])]
                commonPrefixes [] = []
                commonPrefixes ncs = (n, common) : commonPrefixes rest
                    where (n, common, rest) = extractCommonPrefix ncs

                allCommonPrefixes = commonPrefixes recursiveCounts

                safeLookup counts n =
                    case lookup n counts of
                        Just c -> c
                        Nothing -> -1

                constructCommonPrefix (n, sub) =
                    (n, safeLookup counts n, HierarchicalCallCount (construct sub))

                namesWithCount = map fst counts
                namesWithoutChildren =
                    sort namesWithCount \\ sort (map fst allCommonPrefixes)
            in map constructCommonPrefix allCommonPrefixes
                ++ map (\ n -> constructCommonPrefix (n, [])) namesWithoutChildren
    in HierarchicalCallCount $ construct sortedTable

data ProfilingData = ProfilingData {
        totalCallCounts :: CallCounts,
        hierachicalTable :: HierarchicalCallCount
    }

getProfilingData :: EvaluationMonad ProfilingData
getProfilingData = do
    profilerState <- gets profilerState
    return $! unsafePerformIO (extractProfilingData profilerState)
{-# NOINLINE getProfilingData #-}

extractProfilingData :: ProfilerState -> IO ProfilingData
extractProfilingData profilerState = do
    let lock = tableLock profilerState
    (tbl, htbl) <- bracket (takeMVar lock) (\_ -> putMVar lock ()) $ \_ -> do
        tbl <- H.toList (callCounts profilerState)
        htbl <- H.toList (hiearchicalCallCounts profilerState)
        htbl <- mapM (\ (n, table) -> do
            tbl <- H.toList table
            return (n, tbl)) htbl
        return (tbl, htbl)
    return $! ProfilingData tbl (createHierarchicalCallCount htbl)

printInColumns :: [Doc] -> [[Doc]] -> Doc
printInColumns header rows =
    let cols = transpose (map (map show) (header : rows))
        requiredWidth xs = maximum (map length xs)
        padColumn :: Char -> [String] -> [String]
        padColumn c xs = map (\ x -> x ++ replicate (len - length x) c) xs
            where len = requiredWidth xs
        joinColumns :: [[String]] -> [String]
        joinColumns cols = map concatWithSpace (transpose cols)
            where
                concatWithSpace [] = []
                concatWithSpace [x] = x
                concatWithSpace (x:xs) = x ++ " " ++ concatWithSpace xs
        insertSpacer (header:rs) = header : replicate (length header) '-' : rs
    in vcat (map text (insertSpacer (joinColumns (map (padColumn ' ') cols))))

prettyPrintOverallTable :: [(Name, Int)] -> Doc
prettyPrintOverallTable tbl =
    let descendingCounts (_, c1) (_, c2) = compare c2 c1
        totalCallCounts = map (\ (n, c) -> [prettyPrint n, int c]) $!
            sortBy descendingCounts tbl
    in printInColumns [text "Name", text "Call Count"] (totalCallCounts)

prettyPrintHierarchicalData :: HierarchicalCallCount -> Doc
prettyPrintHierarchicalData cs =
    let callCounts :: Int -> HierarchicalCallCount -> [[Doc]]
        callCounts depth (HierarchicalCallCount ncs) =
            concatMap (\ (n, count, rec) ->
                [text (replicate depth ' ') <> prettyPrint n, int count]
                : (callCounts (depth + 2)) rec
                )
            $ sortBy (\ (_, c1, _) (_, c2, _) -> compare c2 c1) ncs
    in printInColumns [text "Name", text "Call Count"] (callCounts 0 cs)

instance PrettyPrintable ProfilingData where
    prettyPrint (ProfilingData tbl hierarchicalData) =
        text "Total Function Call Counts"
        $$ text "--------------------------"
        $$ tabIndent (prettyPrintOverallTable tbl)
        $$ text " "
        $$ text "Hierarchical Call Counts"
        $$ text "------------------------"
        $$ tabIndent (prettyPrintHierarchicalData hierarchicalData)
