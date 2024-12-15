
module Lib3
    ( stateTransition,
    Statements(..),
    Command(..),
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    unmarshallState
    ) where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import System.IO (readFile, writeFile)
import Control.Exception (try, SomeException)
import Control.Concurrent.STM(STM, TVar)
import qualified Lib2
import Data.List ( isPrefixOf )
import GHC.OldList
import Data.Char (isSpace)
import Control.Monad.STM
import Control.Concurrent.STM.TVar

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  -- Read the next operation from the channel
  op <- readChan chan
  case op of
    Save content responseChan -> do
      -- Try to write to the file
      result <- try (writeFile "state.txt" content) :: IO (Either SomeException ())
      case result of
        Right () -> writeChan responseChan () -- Signal success
        Left err -> putStrLn ("Save failed: " ++ show err)
      storageOpLoop chan -- Continue looping

    Load responseChan -> do
      -- Try to read from the file
      result <- try (readFile "state.txt") :: IO (Either SomeException String)
      case result of
        Right content -> writeChan responseChan content -- Send file content
        Left err -> do
          putStrLn ("Load failed: " ++ show err)
          writeChan responseChan "" -- Respond with an empty string on failure
      storageOpLoop chan -- Continue looping

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving Show
-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input =
  case (parseLoadFile `Lib2.or2` parseSaveFile `Lib2.or2` parseStatementsCommand) (Lib2.trim input) of
    Right (command, rest) -> Right (command, rest)
    Left err              -> Left $ "Failed to parse command: " ++ err

-- Parses the load command
parseLoadFile :: String -> Either String (Command, String)
parseLoadFile input =
  case stripPrefix "load" input of
    Just rest -> Right (LoadCommand, Lib2.trim rest)
    Nothing   -> Left "Not a load command"

-- Parses the save command
parseSaveFile :: String -> Either String (Command, String)
parseSaveFile input =
  case stripPrefix "save" input of
    Just rest -> Right (SaveCommand, Lib2.trim rest)
    Nothing   -> Left "Not a save command"

-- Wraps Statements parsing into Command type
parseStatementsCommand :: String -> Either String (Command, String)
parseStatementsCommand input =
  case parseStatements input of
    Right (statements, rest) -> Right (StatementCommand statements, rest)
    Left err                 -> Left $ "Failed to parse statements: " ++ err


parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  case stripPrefix "BEGIN" input of
    Just rest -> parseBatch (dropWhile isSpace rest)
    Nothing -> parseSingle input
  where
    parseSingle:: String -> Either String (Statements, String)
    parseSingle str =
      case Lib2.parseQuery str of
        Right query -> Right (Single query, "")
        Left err -> Left $ "Failed to parse single query: " ++ err

parseBatch :: String -> Either String (Statements, String)
parseBatch str =
  let (queries, remainingLines) = break (== "END") (lines str)
  in case remainingLines of
       [] -> Left "Batch parsing failed: Missing 'END'"
       (_:rest) -> -- Consume "END" and process remaining lines
         parseAllQueries queries >>= \qs ->
           Right (Batch qs, unlines rest)


parseAllQueries:: [String] -> Either String [Lib2.Query]
parseAllQueries[] = Right []
parseAllQueries(q:qs) =
  case Lib2.parseQuery q of
    Right query -> (query :) <$> parseAllQueries qs
    Left err -> Left $ "Batch query parse error: " ++ err

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  let dirs = Lib2.fileSystem state  -- Get the list of directories
      queries = GHC.OldList.concatMap directoryToQueries dirs
  in case queries of
       [singleQuery] -> Single singleQuery
       _             -> Batch queries

-- | Converts a Directory into a list of Queries
directoryToQueries :: Lib2.Directory -> [Lib2.Query]
directoryToQueries dir =
  let dirQuery = Lib2.CreateDirectory (Lib2.DirectoryName (Lib2.directoryName dir))  -- Create the directory
      fileQueries = map (fileToQuery (Lib2.directoryName dir)) (Lib2.files dir)     -- Create all files in the directory
  in dirQuery : fileQueries

-- | Converts a File into a CreateFile Query
fileToQuery :: String -> Lib2.File -> Lib2.Query
fileToQuery dirName file =
  Lib2.CreateFile
    (Lib2.File (Lib2.fileName file) (Lib2.fileSize file)) -- File object
    (Lib2.DirectoryName dirName)                          -- Parent directory name



-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query
renderStatements (Batch queries) = unlines $ ["BEGIN"] ++ map renderQuery queries ++ ["END"]

-- Helper function to render a single query
renderQuery :: Lib2.Query -> String
renderQuery (Lib2.CreateFile file (Lib2.DirectoryName dirName)) =
  "create file " ++ Lib2.fileName file ++ " " ++ show (Lib2.fileSize file) ++ " " ++ dirName
renderQuery (Lib2.CreateDirectory (Lib2.DirectoryName dirName)) =
  "create directory " ++ dirName
renderQuery _ =
  error "Unknown rendering Query"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
-- Update the stateTransition function
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of
  SaveCommand -> do
    state <- readTVarIO stateVar
    let stateAsText = renderStatements (marshallState state)
    putStrLn $ "Saving state: " ++ stateAsText -- Debugging output
    responseChan <- newChan
    writeChan ioChan (Save stateAsText responseChan)
    readChan responseChan -- Wait for save operation to complete
    return $ Right (Just "State saved successfully.")

  LoadCommand -> do
    responseChan <- newChan
    writeChan ioChan (Load responseChan)
    loadedText <- readChan responseChan
    putStrLn $ "Loaded state content: " ++ loadedText -- For debugging
    case parseStatements loadedText of
      Right (parsedStatements, "") -> do
        let newState = unmarshallState parsedStatements
        atomically $ writeTVar stateVar newState
        return $ Right (Just "State loaded successfully.")
      _ -> return $ Left "Failed to parse loaded state."

  StatementCommand statements -> do
    result <- atomically $ applyStatements stateVar statements
    return result

applyStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
applyStatements stateVar (Single query) = do
  state <- readTVar stateVar
  let newState = applyQuery state query
  writeTVar stateVar newState
  return $ Right (Just $ "Executed query: " ++ show query)

applyStatements stateVar (Batch queries) = do
  state <- readTVar stateVar
  let newState = GHC.OldList.foldl applyQuery state queries
  writeTVar stateVar newState
  return $ Right (Just $ "Executed batch of " ++ show (GHC.OldList.length queries) ++ " queries.")

-- | Converts Statements back to the program's State
unmarshallState :: Statements -> Lib2.State
unmarshallState (Single query) = applyQuery Lib2.emptyState query
unmarshallState (Batch queries) = GHC.OldList.foldl applyQuery Lib2.emptyState queries

-- | Apply a single Query to update the state
applyQuery :: Lib2.State -> Lib2.Query -> Lib2.State
applyQuery state (Lib2.CreateDirectory (Lib2.DirectoryName dirName)) =
  state { Lib2.fileSystem = Lib2.fileSystem state ++ [Lib2.Directory dirName []] }
applyQuery state (Lib2.CreateFile file (Lib2.DirectoryName dirName)) =
  let fs = Lib2.fileSystem state
      updatedFS = map (updateDirectory dirName file) fs
  in state { Lib2.fileSystem = updatedFS }
applyQuery state (Lib2.ChangeFileSize (Lib2.DirectoryName dirName) (Lib2.FileName fileName) newSize) =
  let fs = Lib2.fileSystem state
      updatedFS = map (changeFileSizeInDirectory dirName fileName newSize) fs
  in state { Lib2.fileSystem = updatedFS }
applyQuery state (Lib2.DeleteFile (Lib2.FileName fileName) (Lib2.DirectoryName dirName)) =
  let fs = Lib2.fileSystem state
      updatedFS = map (deleteFileFromDirectory dirName fileName) fs
  in state { Lib2.fileSystem = updatedFS }
applyQuery state (Lib2.DeleteDirectory (Lib2.DirectoryName dirName)) =
  let fs = Lib2.fileSystem state
      updatedFS = filter (\dir -> Lib2.directoryName dir /= dirName) fs
  in state {Lib2.fileSystem = updatedFS}
applyQuery state _ = state -- Handle other cases as needed

changeFileSizeInDirectory :: String -> String -> Int -> Lib2.Directory -> Lib2.Directory
changeFileSizeInDirectory targetDirName targetFileName newSize dir
  | Lib2.directoryName dir == targetDirName =
      dir { Lib2.files = map updateFileSize (Lib2.files dir) }
  | otherwise = dir
  where
    updateFileSize file 
      | Lib2.fileName file == targetFileName = file { Lib2.fileSize = newSize }
      | otherwise = file

deleteFileFromDirectory :: String -> String -> Lib2.Directory -> Lib2.Directory
deleteFileFromDirectory targetDirName targetFileName dir
  | Lib2.directoryName dir == targetDirName =
      dir { Lib2.files = filter (\file -> Lib2.fileName file /= targetFileName) (Lib2.files dir) }
  | otherwise = dir

updateDirectory :: String -> Lib2.File -> Lib2.Directory -> Lib2.Directory
updateDirectory targetDirName file dir
  | Lib2.directoryName dir == targetDirName = dir { Lib2.files = Lib2.files dir ++ [file] }
  | otherwise = dir



sampleState :: Lib2.State
sampleState = Lib2.State {
  Lib2.fileSystem = [
    Lib2.Directory {
      Lib2.directoryName = "directoryA",
      Lib2.files = [
        Lib2.File { Lib2.fileName = "fileA", Lib2.fileSize = 10 },
        Lib2.File { Lib2.fileName = "fileB", Lib2.fileSize = 20 }
      ]
    },
    Lib2.Directory {
      Lib2.directoryName = "directoryB",
      Lib2.files = [
        Lib2.File { Lib2.fileName = "fileC", Lib2.fileSize = 30 }
      ]
    }
  ]
}
main :: IO ()
main = do

  -- Testing batch queries
  let inputBatch = unlines
        [ "BEGIN"
        , "create directory directoryA"
        , "create directory directoryB"
        , "create file fileB 200 directoryA"
        , "create file fileA 100 directoryA"
        , "create file fileC 300 directoryB"
        , "END"
        ]
  print ("printing parseCommand")
  print $ parseCommand inputBatch

  print ("printing marshallState")
  let result = marshallState sampleState
  print result

  let rendered = renderStatements result
  putStrLn "Rendered Statements:"
  -- Print the rendered statements to verify correctness
  putStrLn rendered

  case parseStatements rendered of
    Right (parsedStatements, "") -> do
      putStrLn "Parsed Statements:"
      print parsedStatements
      -- Check equivalence
      if parsedStatements == result
        then putStrLn "Parsing succeeded: Original and Parsed Statements match!"
        else putStrLn "Parsing failed: Original and Parsed Statements do not match!"
    Right (_, remaining) ->
      putStrLn $ "Parsing failed: Unconsumed input - " ++ remaining
    Left err ->
      putStrLn $ "Parsing failed with error: " ++ err


  chan <- newChan
  _ <- forkIO $ storageOpLoop chan

  -- Test Save
  responseChan1 <- newChan
  writeChan chan (Save "Hello, World!" responseChan1)
  readChan responseChan1 -- Wait for Save to complete

  -- Test Load
  responseChan2 <- newChan
  writeChan chan (Load responseChan2)
  content <- readChan responseChan2
  putStrLn ("Loaded content: " ++ content) -- Should print "Hello, World!"

