{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Data.Char (toLower)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Text.Printf

import Debug.Trace

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst c (c':cs) | c == c' = ([], cs)
splitOnFirst c (c':cs) = (c':xs, ys)
    where (xs, ys) = splitOnFirst c cs

parseCSPMIdentifierFile :: String -> M.Map String String
parseCSPMIdentifierFile file = M.fromList $ catMaybes $ map parseLine (lines file)
    where
        parseLine [] = Nothing
        parseLine line = Just ("cspm_"++strNum, str)
            where
                (strNum, str) = splitOnFirst ' ' line

data CostCenterProfile = CostCenterProfile {
        costCenterName :: String,
        costCenterEntriesCount :: Int,
        costCenterIndividualTimePercentge :: Float,
        costCenterIndividualAllocPercentage :: Float,
        costCenterInheritedTimePercentge :: Float,
        costCenterInheritedAllocPercentge :: Float,
        costCenterChildren :: [CostCenterProfile]
    }
    deriving Show

instance Eq CostCenterProfile where
    p1 == p2 = costCenterName p1 == costCenterName p2

instance Ord CostCenterProfile where
    compare p1 p2 = compare (costCenterName p1) (costCenterName p2)

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t'

isBlank :: String -> Bool
isBlank xs = and (map isSpace xs)

-- | Removes the indent from a string, returning the length of it, and the
-- remainder of the line.
extractIndent :: String -> (Int, String)
extractIndent s = (length prefix, suffix)
    where (prefix, suffix) = span isSpace s

-- | Removes spaces from the left side.
ltrim :: String -> String
ltrim = snd . extractIndent

parseHaskellProfile :: String -> ([CostCenterProfile], String, String)
parseHaskellProfile file =
    let allLines = lines file
        -- We need to skip the header lines until we reach the header line

        skipUntil 2 lines = lines
        skipUntil n (line:lines) | isBlank line = skipUntil (n+1) lines
        skipUntil _ (_:lines) = skipUntil 0 lines

        bodyLines = drop 3 $ skipUntil 0 allLines

        headerLines = take (length allLines - length bodyLines) allLines

        extractColumn :: String -> (String, String)
        extractColumn line = span (not . isSpace) (dropWhile isSpace line)

        parseLine :: String -> (CostCenterProfile, Int)
        parseLine line =
            let
                (indent, rest0) = extractIndent line
                (nameStr, rest1) = extractColumn rest0
                (moduleStr, rest2) = extractColumn rest1
                (costCenterIdStr, rest3) = extractColumn rest2
                (entriesStr, rest4) = extractColumn rest3
                (indivTimeStr, rest5) = extractColumn rest4
                (indivAllocStr, rest6) = extractColumn rest5
            in (CostCenterProfile {
                        costCenterName = nameStr,
                        costCenterEntriesCount = read entriesStr,
                        costCenterIndividualTimePercentge = read indivTimeStr,
                        costCenterIndividualAllocPercentage = read indivAllocStr,
                        costCenterInheritedTimePercentge = 0.0,
                        costCenterInheritedAllocPercentge = 0.0,
                        costCenterChildren = []
                    }, indent)

        makeHierarchy :: [(CostCenterProfile, Int)] -> [CostCenterProfile]
        makeHierarchy ((c, i):xs) = makeHierarchy' [(c, i)] xs

        makeHierarchy' :: [(CostCenterProfile, Int)] ->
            [(CostCenterProfile, Int)] -> [CostCenterProfile]
        makeHierarchy' stk [] = popHierarchy stk []
        makeHierarchy' stk@((currentCostCenter, currentIndent):_) 
                        remainder@((nextCostCenter, nextIndent):ns) =
            if nextIndent > currentIndent then
                -- next is our child
                makeHierarchy' ((nextCostCenter, nextIndent):stk) ns
            else
                popHierarchy stk remainder

        popHierarchy ((child, _):(parent, parentIndent):rest) remainder =
            let stk' = (parent { costCenterChildren = child : costCenterChildren parent }, parentIndent)
                        : rest
            in makeHierarchy' stk' remainder
        -- if x has no parent, then it must be a top level cost center, and
        -- y must be its sibling, so pop x
        popHierarchy [(x, _)] (y:ys) = x : makeHierarchy' [y] ys
        popHierarchy [(x, _)] [] = [x]

        totalRuntimeString = ltrim $ drop 1 $ ltrim $ drop (length "total time") $
            ltrim $ head $ dropWhile (\ s -> not ("total time" `isInfixOf` s)) headerLines
        totalAllocationsString = ltrim $ drop 1 $ ltrim $ drop (length "total alloc") $
            ltrim $ head $ dropWhile (\ s -> not ("total alloc" `isInfixOf` s)) headerLines

    in (makeHierarchy $ map parseLine bodyLines,
        totalRuntimeString, totalAllocationsString)

substituteNames :: M.Map String String -> CostCenterProfile -> CostCenterProfile
substituteNames nameMap profile =
    profile {
        costCenterName = case M.lookup (costCenterName profile) nameMap of
                            Just n' -> n'
                            Nothing -> costCenterName profile
        , costCenterChildren = map (substituteNames nameMap) (costCenterChildren profile)
    }

fixIndividualPercentages :: [CostCenterProfile] -> [CostCenterProfile]
fixIndividualPercentages profiles =
    let
        calc profile = (
                costCenterIndividualTimePercentge profile + childPercentageTime,
                costCenterIndividualAllocPercentage profile + childAllocTime
            )
            where
                childPercentages = map calc (costCenterChildren profile)
                (childTimes, childAllocs) = unzip childPercentages
                childPercentageTime = sum childTimes
                childAllocTime = sum childAllocs

        allPercentages = map calc profiles
        (times, allocs) = unzip allPercentages
        totalPercentageTime = sum times
        totalPercentageAlloc = sum allocs

        fix profile =
            profile {
                costCenterIndividualTimePercentge =
                    100.0 * (costCenterIndividualTimePercentge profile
                            / totalPercentageTime)
                , costCenterIndividualAllocPercentage =
                    100.0 * (costCenterIndividualAllocPercentage profile
                            / totalPercentageAlloc)
                , costCenterChildren = map fix (costCenterChildren profile)
            }
    in map fix profiles

calculateInheritedPercentages :: CostCenterProfile -> CostCenterProfile
calculateInheritedPercentages profile =
    profile {
        costCenterInheritedTimePercentge =
            costCenterIndividualTimePercentge profile
            + sum (map costCenterInheritedTimePercentge newChildren)
        , costCenterInheritedAllocPercentge =
            costCenterIndividualAllocPercentage profile
            + sum (map costCenterInheritedAllocPercentge newChildren)
        , costCenterChildren = newChildren
    }
    where
        newChildren = map calculateInheritedPercentages (costCenterChildren profile)

findTopFunctions :: [CostCenterProfile] -> [CostCenterProfile]
findTopFunctions profiles =
    let
        flatten profile = profile : concatMap flatten (costCenterChildren profile)
        flatProfiles = concatMap flatten profiles

        combineProfiles :: [CostCenterProfile] -> CostCenterProfile
        combineProfiles [p] = p
        combineProfiles ps@(repr:_) = CostCenterProfile {
                costCenterName = costCenterName repr,
                costCenterEntriesCount = sum (map costCenterEntriesCount ps),
                costCenterIndividualTimePercentge =
                    sum (map costCenterIndividualTimePercentge ps),
                costCenterIndividualAllocPercentage =
                    sum (map costCenterIndividualAllocPercentage ps),
                costCenterInheritedTimePercentge =
                    sum (map costCenterInheritedTimePercentge ps),
                costCenterInheritedAllocPercentge =
                    sum (map costCenterInheritedAllocPercentge ps),
                costCenterChildren = []
            }

        sortProfiles =
            sortBy (\ p1 p2 ->
                compare (costCenterIndividualTimePercentge p2)
                        (costCenterIndividualTimePercentge p1))
    in sortProfiles $ map combineProfiles $ group $ sort flatProfiles

printProfile :: [CostCenterProfile] -> String -> String -> String
printProfile ps totalRuntime totalAllocations =
    let
        padString target str = str ++ (replicate (target-length str) ' ')

        maxNameColumnWidth profile = length (costCenterName profile) `max`
            case costCenterChildren profile of
                [] -> 0
                cs -> 1 + maximum (map maxNameColumnWidth cs)
        maxEntryCountWidth profile = length (show $ costCenterEntriesCount profile) `max`
            case costCenterChildren profile of
                [] -> 0
                cs -> 1 + maximum (map maxEntryCountWidth cs)

        costCenterWidth =
            length "COST CENTRE " `max` (2 + maximum (map maxNameColumnWidth ps))
        entryCountWidth =
            length "ENTRIES " `max` (4 + maximum (map maxEntryCountWidth ps))
        percentageWidth = length "%alloc"+1

        float = printf "%.1f"

        sortProfiles = sortBy (\ p1 p2 ->
            case compare (costCenterInheritedTimePercentge p2)
                    (costCenterInheritedTimePercentge p1) of
                EQ -> compare (map toLower $ costCenterName p1)
                                (map toLower $ costCenterName p2)
                x -> x)

        printRow :: Int -> CostCenterProfile -> String
        printRow currentIndent p =
            replicate currentIndent ' '
            ++ padString (costCenterWidth - currentIndent) (costCenterName p)
            ++ padString entryCountWidth (show $ costCenterEntriesCount p)
            ++ padString percentageWidth (float $ costCenterIndividualTimePercentge p)
            ++ padString (percentageWidth+4) (float $ costCenterIndividualAllocPercentage p)
            ++ padString percentageWidth (float $ costCenterInheritedTimePercentge p)
            ++ padString percentageWidth (float $ costCenterInheritedAllocPercentge p)
            ++ "\n"
            ++ concatMap (printRow (currentIndent + 1)) (sortProfiles $ costCenterChildren p)

        printSummary :: CostCenterProfile -> String
        printSummary p =
            padString costCenterWidth (costCenterName p)
            ++ padString entryCountWidth (show $ costCenterEntriesCount p)
            ++ padString percentageWidth (float $ costCenterIndividualTimePercentge p)
            ++ padString (percentageWidth+4) (float $ costCenterIndividualAllocPercentage p)
            ++ "\n"

    in 
          "TOTAL RUNTIME     "++totalRuntime++"\n"
        ++"TOTAL ALLOCATIONS "++totalAllocations++"\n\n\n"
        ++"TOP FUNCTIONS\n\n"
        ++ padString costCenterWidth "COST CENTRE"
        ++ padString entryCountWidth "ENTRIES"
        ++ padString percentageWidth "%time"
        ++ padString (percentageWidth+4) "%alloc"
        ++ "\n"
        ++ concatMap printSummary (take 10 (findTopFunctions ps))

        ++ "\n\nALL FUNCTIONS\n\n"
        ++ padString costCenterWidth "COST CENTRE"
        ++ padString entryCountWidth "ENTRIES"
        ++ padString (percentageWidth+percentageWidth+4) (" individual")
        ++ padString (percentageWidth+percentageWidth) ("  inherited")
        ++ "\n"
        ++ padString costCenterWidth ""
        ++ padString entryCountWidth ""
        ++ padString percentageWidth "%time"
        ++ padString (percentageWidth+4) "%alloc"
        ++ padString percentageWidth "%time"
        ++ padString percentageWidth "%alloc"
        ++ "\n"
        ++ concatMap (printRow 0) (sortProfiles ps)

isCSPMCostCenter :: CostCenterProfile -> Bool
isCSPMCostCenter p = "cspm_" `isPrefixOf` (costCenterName p)

restrictProfileToCSPM :: CostCenterProfile -> [CostCenterProfile]
restrictProfileToCSPM p | not (isCSPMCostCenter p) =
    concatMap restrictProfileToCSPM (costCenterChildren p)
restrictProfileToCSPM p = [p {
        costCenterChildren = concatMap restrictProfileToCSPM (costCenterChildren p)
    }]

filterCheapFunctions :: CostCenterProfile -> [CostCenterProfile]
filterCheapFunctions profile =
    if costCenterIndividualTimePercentge profile == 0.0
            && costCenterIndividualAllocPercentage profile == 0.0
            && costCenterInheritedTimePercentge profile == 0.0
            && costCenterIndividualAllocPercentage profile == 0.0 then
        []
    else
        [profile { costCenterChildren = newChildren }]
    where
        newChildren = concatMap filterCheapFunctions (costCenterChildren profile)

processProfile :: Options -> String -> String -> IO ()
processProfile options profileFile cspmFile =
    let cspmIdentMap = parseCSPMIdentifierFile cspmFile
        (parsedProfile, totalRuntime, totalAllocations) =
                parseHaskellProfile profileFile
        restrictedProfile = concatMap restrictProfileToCSPM parsedProfile
        substitutedProfile = map (substituteNames cspmIdentMap) restrictedProfile
        fixedPercentages = fixIndividualPercentages substitutedProfile
        correctedProfile = map calculateInheritedPercentages fixedPercentages
        filteredProfile =
            if filterCheap options then 
                concatMap filterCheapFunctions correctedProfile
            else
                correctedProfile
    in putStr $ "\n\n\n\n"
        ++printProfile filteredProfile totalRuntime totalAllocations

profileFile :: Options -> FilePath -> IO ()
profileFile options fp = do
    fullFilePath <- canonicalizePath fp
    executablePath <- getExecutablePath
    let explorerExecutable = replaceFileName executablePath $ "cspmexplorerprof"
#ifdef mingw32_HOST_OS
                                                                ++ ".exe"
#endif
        profFileName = "cspmexplorerprof.prof"
        cspmFileName = "cspmexplorerprof.cspm"
        explorerArgs = [fullFilePath, "+RTS", "-p"]
    withSystemTempDirectory "cspmprofilerXXXXXXXX" $! \ tempDir -> do
        --putStrLn $! "Using "++show tempDir
        --putStrLn $! show explorerArgs
        (_, _, _, process) <- createProcess $
            (proc explorerExecutable explorerArgs) {
                cwd = Just tempDir
            }
        waitForProcess process
        profileFileContents <- readFile (joinPath [tempDir, profFileName])
        cspmFileContents <- readFile (joinPath [tempDir, cspmFileName])
        length profileFileContents `seq` length cspmFileContents `seq`
            processProfile options profileFileContents cspmFileContents

printError :: String -> IO ()
printError s = putStrLn $ "\ESC[1;31m\STX"++s++"\ESC[0m\STX"

data Options = Options {
        help :: Bool,
        filterCheap :: Bool
    }

defaultOptions :: Options
defaultOptions = Options { 
        help = False,
        filterCheap = False
    }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] 
        (NoArg (\o -> o { help = True })) 
        "Display usage message",
    Option [] ["filter-unimportant"]
        (NoArg (\o -> o { filterCheap = True }))
        "Filters functions from the profile that use insignificant resources."
    ]

header :: String
header = "Usage: cspmprofiler [OPTION...] file"

main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        (_,_,e:es) -> liftIO $ putStr $ concat (e:es) ++ usageInfo header options
        (o,files, []) -> do
            let opts = foldl (flip id) defaultOptions o
            case (opts, files) of
                (Options { help = True }, []) -> 
                    liftIO $ putStr $ usageInfo header options
                (_, [f]) -> profileFile opts f
                (_, _) -> liftIO $ putStr $ usageInfo header options
