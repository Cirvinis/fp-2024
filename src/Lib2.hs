{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib2
    ( Query(..),
    parseQuery,
    parseFile,
    parseDirectory,
    parseContents,
    parseFileSystem,
    parseCreateFile,
    parseCreateDirectory,
    parseDeleteFile,
    parseDeleteDirectory,
    parseChangeFileSize,
    parseShowDirectory,
    parseShowFileSystem,
    State(..),
    emptyState,
    stateTransition,
    File(..),
    FileName(..),
    DirectoryName(..),
    Directory(..),
    FileSystem(..),
    ) where


import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

data FileName = FileName{
  fileName':: String
} deriving Show

data DirectoryName = DirectoryName{
  directoryName':: String
} deriving Show

data Directory = Directory{
  directoryName::String,
  files:: [File]
}

data File = File{
  fileName:: String,
  fileSize:: Int
}

data FileSystem = FileSystem{
  directories::[Directory]
}

data Query = 
  CreateFile File DirectoryName |
  CreateDirectory DirectoryName |
  DeleteFile FileName DirectoryName |
  DeleteDirectory DirectoryName |
  ChangeFileSize DirectoryName FileName Int |
  ShowDirectory DirectoryName |
  ShowFileSystem FileSystem |
  View
  deriving Show

dropWhitespace :: String -> String
dropWhitespace = dropWhile C.isSpace

parseWord :: Parser String
parseWord input =
  let word = L.takeWhile C.isLetter (dropWhitespace input)
      rest = dropWhitespace (drop (length word) input)
  in if not (null word)
       then Right (word, rest)
       else Left "Expected a word (sequence of letters)"

parseNumber :: Parser Int
parseNumber input =
  let digits = L.takeWhile C.isDigit (dropWhitespace input)
      rest = dropWhitespace (drop (length digits) input)
  in if not (null digits)
       then Right (read digits, rest)
       else Left "Expected a number"

instance Eq Query where
  (CreateFile file dir) == (CreateFile file' dir') = file == file' && dir == dir'
  (CreateDirectory dir) == (CreateDirectory dir') = dir == dir'
  (DeleteFile file dir) == (DeleteFile file' dir') = file == file' && dir == dir'
  (DeleteDirectory dir) == (DeleteDirectory dir') = dir == dir'
  (ChangeFileSize dir file size) == (ChangeFileSize dir' file' size') = dir == dir' && file == file' && size == size'
  (ShowDirectory dir) == (ShowDirectory dir') = dir == dir'
  (View) == (View) = True
  _ == _ = False


instance Eq Lib2.File where
  (Lib2.File name size) == (Lib2.File name' size') = name == name' && size == size'

instance Eq Lib2.DirectoryName where
  (Lib2.DirectoryName dir) == (Lib2.DirectoryName dir') = dir == dir'

instance Eq Lib2.FileName where
  (Lib2.FileName name) == (Lib2.FileName name') = name == name'




-- Show instance for File
instance Show File where
  show (File fileName fileSize) =
    "File: " ++ fileName ++ " (Size: " ++ show fileSize ++ " bytes)"

-- Show instance for Directory
instance Show Directory where
  show (Directory dirName files) =
    "Directory: " ++ dirName ++ "\n" ++
    if null files
      then "  No files"
      else unlines (map (("  " ++) . show) files)

-- Show instance for FileSystem
instance Show FileSystem where
  show (FileSystem directories) =
    "FileSystem contains:\n" ++
    if null directories
      then "  No directories"
      else unlines (map (("  " ++) . show) directories)

parseFile :: Parser File
parseFile input = 
  case parseWord input of
    Left err -> Left err
    Right (name, restAfterName) ->
      case parseNumber restAfterName of
        Left err -> Left err
        Right (size, restAfterNumber) ->
          Right(File name size, restAfterNumber)

parseDirectory :: Parser Directory
parseDirectory =
  and2
    (\dirName givenFiles -> Directory dirName givenFiles)
    parseWord
    parseContents

parseContents :: Parser [File]
parseContents = many parseFile

parseFileSystem :: Parser [Directory]
parseFileSystem = many (parseDirectory)

many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r


or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 = \input -> case p1 input of
  Right result -> Right result
  Left _       -> p2 input

and1:: (a -> b) -> Parser a -> Parser b
and1 f p1 input =
  case p1 input of
    Right(result1, rest1) -> Right(f result1, rest1)
    Left err -> Left err

and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f p1 p2 input =
    case p1 input of
        Right (result1, rest1) ->
            case p2 rest1 of
                Right (result2, rest2) -> Right (f result1 result2, rest2)
                Left err -> Left err
        Left err -> Left err
  
and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f p1 p2 p3 input =
  case p1 input of
    Right (result1, rest1) ->
      case p2 rest1 of
        Right (result2, rest2) ->
          case p3 rest2 of
            Right (result3, rest3) -> Right (f result1 result2 result3, rest3)
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f p1 p2 p3 p4 input =
  case p1 input of
    Right (result1, rest1) ->
      case p2 rest1 of
        Right (result2, rest2) ->
          case p3 rest2 of
            Right (result3, rest3) ->
              case p4 rest3 of
                Right (result4, rest4) -> Right (f result1 result2 result3 result4, rest4)
                Left err -> Left err
            Left err -> Left err
        Left err -> Left err
    Left err -> Left err

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Expected '" ++ [c] ++ "', but found end of input")
parseChar c (x:xs)
  | c == x    = Right (c, xs)
  | otherwise = Left ("Expected '" ++ [c] ++ "', but found '" ++ [x] ++ "'")

string :: String -> Parser String
string [] input = Right ([], input)
string (c:cs) input = case parseChar c input of
  Right (_, rest) -> 
    case string cs rest of
      Right (res, rest') -> Right (c:res, rest')
      Left err -> Left err
  Left err -> Left err

parseCreateFile:: Parser Query
parseCreateFile =
  and3
    (\_ givenFile givenDirectoryName -> CreateFile givenFile (DirectoryName givenDirectoryName))
    (string "create file ")
    parseFile
    parseWord

parseCreateDirectory:: Parser Query
parseCreateDirectory =
  and2
  (\_ givenDirectoryName -> CreateDirectory (DirectoryName givenDirectoryName))
  (string "create directory ")
  parseWord

parseDeleteFile:: Parser Query
parseDeleteFile =
  and3
    (\_ givenFileName givenDirectoryName -> DeleteFile (FileName givenFileName)  (DirectoryName givenDirectoryName))
    (string "delete file ")
    parseWord
    parseWord

parseDeleteDirectory:: Parser Query
parseDeleteDirectory =
  and2
  (\_ givenDirectoryName -> DeleteDirectory (DirectoryName givenDirectoryName))
  (string "delete directory")
  parseWord

parseChangeFileSize:: Parser Query
parseChangeFileSize = 
  and4
  (\_ givenDirectoryName givenFileName newSize -> ChangeFileSize (DirectoryName givenDirectoryName) (FileName givenFileName) newSize)
  (string "change file size ")
  parseWord
  parseWord
  parseNumber

parseShowDirectory:: Parser Query
parseShowDirectory =
  and2
  (\_ givenDirectoryName -> ShowDirectory (DirectoryName givenDirectoryName))
  (string "show directory ")
  parseWord

parseShowFileSystem :: Parser Query
parseShowFileSystem =
  and2
  (\_ givenDirectories-> ShowFileSystem (FileSystem givenDirectories))
  (string "show filesystem ")
  parseFileSystem

parseView :: Parser Query
parseView = 
  and1
    (\_-> View)  -- Return the View command when parsed
    (string "view")  -- Expect the word "view"


-- The function must have tests.
parseQuery:: String -> Either String Query
parseQuery input = 
  case (parseCreateFile `or2`
   parseCreateDirectory `or2`
   parseDeleteFile `or2`
   parseDeleteDirectory `or2`
   parseChangeFileSize `or2`
   parseShowDirectory `or2`
   parseShowFileSystem `or2`
   parseView) (trim input) of
    Right (query, _) -> Right query
    Left err -> Left err

-- Function to trim whitespace from both ends of a string
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile C.isSpace

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
  fileSystem :: [Directory]  -- List of top-level directories
} deriving Show

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {
  fileSystem = []
}
-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (CreateDirectory (DirectoryName dirName)) =
  if any ((== dirName) . directoryName) (fileSystem state)
  then Left ("Directory " ++ dirName ++ " already exists.")
  else
    let newDir = Directory { directoryName = dirName, files = [] }
        newFileSystem = newDir : fileSystem state
    in Right (Just ("Directory " ++ dirName ++ " created."), state { fileSystem = newFileSystem })

stateTransition state (CreateFile file (DirectoryName dirName)) =
  let updateDirectory dir
        | directoryName dir == dirName = dir { files = file : files dir }
        | otherwise = dir
      updatedFileSystem = map updateDirectory (fileSystem state)
  in if any ((== dirName) . directoryName) (fileSystem state)
     then Right (Just ("File " ++ fileName file ++ " of size " ++ show (fileSize file) 
                        ++ " created in directory " ++ dirName ++ "."), 
                 state { fileSystem = updatedFileSystem })
     else Left ("Directory " ++ dirName ++ " does not exist.")

stateTransition state (DeleteFile (FileName fileName') (DirectoryName dirName)) =
  let findDirectory = filter ((== dirName) . directoryName) (fileSystem state)
  in case findDirectory of
       [dir] -> -- Directory exists
         let updatedFiles = filter ((/= fileName') . fileName) (files dir)
         in if length (files dir) == length updatedFiles
            then Left ("File " ++ fileName' ++ " does not exist in directory " ++ dirName ++ ".")
            else Right (Just ("File " ++ fileName' ++ " deleted from directory " ++ dirName ++ "."), 
                        state { fileSystem = map (\d -> if directoryName d == dirName then d { files = updatedFiles } else d) (fileSystem state) })
       [] -> Left ("Directory " ++ dirName ++ " does not exist.")


stateTransition state (DeleteDirectory (DirectoryName dirName)) =
  let updatedFileSystem = filter ((/= dirName) . directoryName) (fileSystem state)
  in if any ((== dirName) . directoryName) (fileSystem state)
     then Right (Just ("Directory " ++ dirName ++ " deleted."), state { fileSystem = updatedFileSystem })
     else Left ("Directory " ++ dirName ++ " does not exist.")

stateTransition state (ChangeFileSize (DirectoryName dirName) (FileName fileName') newSize) =
  let updateDirectory dir
        | directoryName dir == dirName =
            let updateFile file
                  | fileName file == fileName' = file { fileSize = newSize }
                  | otherwise = file
                updatedFiles = map updateFile (files dir)
            in dir { files = updatedFiles }
        | otherwise = dir
      updatedFileSystem = map updateDirectory (fileSystem state)
  in if any ((== dirName) . directoryName) (fileSystem state)
     then Right (Just ("File " ++ fileName' ++ " size changed to " ++ show newSize ++ " in directory " ++ dirName ++ "."), state { fileSystem = updatedFileSystem })
     else Left ("Directory " ++ dirName ++ " does not exist.")

stateTransition state (ShowDirectory (DirectoryName dirName)) =
  let findDirectory = filter ((== dirName) . directoryName) (fileSystem state)
  in case findDirectory of
       (dir:_) -> Right (Just ("Directory " ++ dirName ++ " contains: " ++ show (files dir)), state)
       [] -> Left ("Directory " ++ dirName ++ " does not exist.")

stateTransition state (ShowFileSystem _) =
  Right (Just ("Current file system: " ++ show (fileSystem state)), state)

stateTransition state (View) =
  Right (Just ("Current file system:\n" ++ show (fileSystem state)), state)