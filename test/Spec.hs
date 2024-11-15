{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "CreateFile - correct format" $
      Lib2.parseQuery "create file example.txt 100 /docs" @?= 
      Right (Lib2.CreateFile (Lib2.File {Lib2.fileName = "example.txt", Lib2.fileSize = 100}) (Lib2.DirectoryName "/docs")),

      
    testCase "CreateDirectory - correct format" $
      Lib2.parseQuery "create directory /images" @?= 
      Right (Lib2.CreateDirectory (Lib2.DirectoryName "/images")),
      
    testCase "DeleteFile - correct format" $
      Lib2.parseQuery "delete file example.txt /docs" @?= 
      Right (Lib2.DeleteFile (Lib2.FileName "example.txt") (Lib2.DirectoryName "/docs")),
      
    testCase "DeleteDirectory - correct format" $
      Lib2.parseQuery "delete directory /images" @?= 
      Right (Lib2.DeleteDirectory (Lib2.DirectoryName "/images")),
      
    testCase "ChangeFileSize - correct format" $
      Lib2.parseQuery "change file size /docs example.txt 200" @?= 
      Right (Lib2.ChangeFileSize (Lib2.DirectoryName "/docs") (Lib2.FileName "example.txt") 200),
      
    testCase "ShowDirectory - correct format" $
      Lib2.parseQuery "show directory /docs" @?= 
      Right (Lib2.ShowDirectory (Lib2.DirectoryName "/docs")),
      
    testCase "ShowFileSystem - correct format" $
      Lib2.parseQuery "show filesystem" @?= 
      Right (Lib2.ShowFileSystem (Lib2.FileSystem [])),  -- adjust as needed for expected empty filesystem format
      
    testCase "View - correct format" $
      Lib2.parseQuery "view" @?= 
      Right Lib2.View,

    -- Invalid input test cases
    testCase "CreateFile - incorrect format" $
      Lib2.parseQuery "create file /docs 100" @?= 
      Left "Expected a word (sequence of letters)",

    testCase "ChangeFileSize - missing size" $
      Lib2.parseQuery "change file size /docs example.txt" @?= 
      Left "Expected a number"
  ]