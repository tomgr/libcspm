module CSPM.Parser.Monad (
    ParseMonad, ParserState(..), 
    FileParserState(..), movePos,
    setParserState, getParserState, 
    FilePosition(..), filePositionToSrcLoc,
    modifyTopFileParserState, getTopFileParserState,
    
    runParser, pushFile, pushFileContents,
    getTokenizerPos, getFileName, getInput, 
    getPreviousChar, getCurrentStartCode, setCurrentStartCode, 
    getSequenceStack, setSequenceStack
)
where

import Control.Exception
import Control.Monad.State
import Prelude hiding (catch)
import System.FilePath

import CSPM.Parser.Exceptions
import Util.Annotated

-- *************************************************************************
-- Parser Support Types
-- *************************************************************************

-- TAKEN FROM ALEX
-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.
data FilePosition = FilePosition !Int !Int !Int
    deriving (Eq,Show)

startPos :: FilePosition
startPos = FilePosition 0 1 1

filePositionToSrcLoc :: String -> FilePosition -> SrcSpan
filePositionToSrcLoc filePath (FilePosition _ line col) = 
    SrcSpanPoint filePath line col
    
movePos :: FilePosition -> Char -> FilePosition
-- Don't treat tabs any differently (editors don't)
--movePos (FilePosition a l c) '\t' = 
--  FilePosition (a+1)  l     (((c+7) `div` 8)*8+1)
movePos (FilePosition a l c) '\n' = FilePosition (a+1) (l+1)   1
movePos (FilePosition a l c) _    = FilePosition (a+1)  l     (c+1)

-- *************************************************************************
-- Parser Monad
-- *************************************************************************
data ParserState = ParserState {
        rootDir :: !String,
        fileStack :: ![FileParserState]
    }
    deriving Show

data FileParserState = FileParserState {
        tokenizerPos :: !FilePosition,
        fileName :: !String,
        input :: String,
        previousChar :: !Char,
        currentStartCode :: !Int, -- current startcode
        
        sequenceStack :: ![Int]
    } 
    deriving Show

-- The parser monad takes a sequence of parser states, representing the
-- current stack of include files
type ParseMonad = StateT ParserState IO

runParser :: ParseMonad a -> String -> IO a
runParser prog dirname =
    runStateT prog (ParserState dirname [])
    >>= return . fst

getTopFileParserState :: ParseMonad FileParserState
getTopFileParserState = gets (head . fileStack)

getParserState :: ParseMonad ParserState
getParserState = gets id

setParserState :: ParserState -> ParseMonad ()
setParserState st = modify (\ _ -> st)

modifyTopFileParserState :: (FileParserState -> FileParserState) -> ParseMonad ()
modifyTopFileParserState stf =
    modify (\ st -> let fs:fss = fileStack st in 
                        st { fileStack = (stf fs):fss })

pushFile :: String -> ParseMonad a -> ParseMonad a
pushFile fname prog = do
    dirname <- gets rootDir     
    let 
        filename = combine dirname fname
        handle :: IOException -> a
        handle err = throwSourceError [fileAccessErrorMessage filename]
    str <- liftIO $ catch (readFile filename) handle
    pushFileContents filename str
    x <- prog
    return x

pushFileContents :: String -> String -> ParseMonad ()
pushFileContents filename input = 
    modify (\ st -> let
            fs = FileParserState startPos filename input '\n' 0 [0]
        in
            st { fileStack = fs:(fileStack st) })

setFileParserState :: FileParserState -> ParseMonad ()
setFileParserState fs = 
    modify (\ st -> st { fileStack = fs:(tail (fileStack st)) } )

getTokenizerPos :: ParseMonad FilePosition
getTokenizerPos = getTopFileParserState >>= (return . tokenizerPos)

getFileName :: ParseMonad String
getFileName = getTopFileParserState >>= (return . fileName)

getInput :: ParseMonad String
getInput = getTopFileParserState >>= (return . input)

getPreviousChar :: ParseMonad Char
getPreviousChar = getTopFileParserState >>= (return . previousChar)

getCurrentStartCode :: ParseMonad Int
getCurrentStartCode = getTopFileParserState >>= (return . currentStartCode)

getSequenceStack :: ParseMonad [Int]
getSequenceStack = getTopFileParserState >>= (return . sequenceStack)

setCurrentStartCode :: Int -> ParseMonad ()
setCurrentStartCode sc = 
    modifyTopFileParserState (\ st -> st { currentStartCode = sc })

setSequenceStack :: [Int] -> ParseMonad ()
setSequenceStack st = 
    modifyTopFileParserState (\ s -> s { sequenceStack = st })
