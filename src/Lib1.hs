module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
    "Directory: \"Root\""
    , "Directory: \"Documents\""
    , "Directory: \"Photos\""
    , "File: \"README.txt\" Size: 123"
    , "File: \"Resume.docx\" Size: 456"
    , "File: \"Vacation.jpg\" Size: 789"
    , "File: \"NewYear.png\" Size: 1011"
    , "Directory: \"2024\""
    ]
