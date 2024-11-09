module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["create file ", "create directory ", "delete file ", "delete directory ", "change file size ", "show directory ", "show filesystem ", "view"]
