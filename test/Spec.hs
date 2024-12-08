{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Control.Concurrent.STM (STM, TVar)
import Control.Concurrent (forkIO)

import Data.List
import Data.Ord

import Lib2 qualified
import Lib3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "CreateFile - correct format" $
      Lib2.parseQuery "create file fileA 100 directoryA" @?= 
      Right (Lib2.CreateFile (Lib2.File {Lib2.fileName = "fileA", Lib2.fileSize = 100}) (Lib2.DirectoryName "directoryA")),

      
    testCase "CreateDirectory - correct format" $
      Lib2.parseQuery "create directory directoryA" @?= 
      Right (Lib2.CreateDirectory (Lib2.DirectoryName "directoryA")),
      
    testCase "DeleteFile - correct format" $
      Lib2.parseQuery "delete file fileA directoryA" @?= 
      Right (Lib2.DeleteFile (Lib2.FileName "fileA") (Lib2.DirectoryName "directoryA")),
      
    testCase "DeleteDirectory - correct format" $
      Lib2.parseQuery "delete directory directoryA" @?= 
      Right (Lib2.DeleteDirectory (Lib2.DirectoryName "directoryA")),
      
    testCase "ChangeFileSize - correct format" $
      Lib2.parseQuery "change file size directoryA fileA 200" @?= 
      Right (Lib2.ChangeFileSize (Lib2.DirectoryName "directoryA") (Lib2.FileName "fileA") 200),
      
    testCase "ShowDirectory - correct format" $
      Lib2.parseQuery "show directory directoryA" @?= 
      Right (Lib2.ShowDirectory (Lib2.DirectoryName "directoryA")),  -- adjust as needed for expected empty filesystem format
      
    testCase "View - correct format" $
      Lib2.parseQuery "view" @?= 
      Right Lib2.View

    -- -- Invalid input test cases
    -- testCase "CreateFile - incorrect format" $
    --   Lib2.parseQuery "create file /docs 100" @?= 
    --   Left "Expected a word (sequence of letters)",

    -- testCase "ChangeFileSize - missing size" $
    --   Lib2.parseQuery "change file size /docs example.txt" @?= 
    --   Left "Expected a number"
  ]

-- | **Generators**
genFileName :: Gen Lib2.FileName
genFileName = Lib2.FileName <$> listOf1 (elements ['a'..'z'])

genDirectoryName :: Gen Lib2.DirectoryName
genDirectoryName = Lib2.DirectoryName <$> listOf1 (elements ['a'..'z'])

genFile :: Gen Lib2.File
genFile = Lib2.File <$> (Lib2.fileName' <$> genFileName) <*> choose (1, 100) -- File size between 1 and 100


genFileSystem :: Int -> Gen Lib2.FileSystem
genFileSystem depth 
  | depth <= 0 = Lib2.SingleDirectory <$> genDirectory
  | otherwise = 
      oneof [ Lib2.SingleDirectory <$> genDirectory  -- Single directory
            , Lib2.DirectoryWithRest <$> genDirectory <*> genFileSystem (depth - 1)  -- Recursive case, decrease depth
            ]


genDirectory :: Gen Lib2.Directory
genDirectory = Lib2.Directory <$> (Lib2.directoryName' <$> genDirectoryName) <*> listOf genFile

genQuery :: Gen Lib2.Query
genQuery = oneof 
  [ Lib2.CreateFile <$> genFile <*> genDirectoryName
  , Lib2.CreateDirectory <$> genDirectoryName
  ]

genStatements :: Gen Lib3.Statements
genStatements = do
  queries <- listOf1 genQuery
  case queries of
    [q] -> return $ Lib3.Single q
    qs  -> return $ Lib3.Batch qs

-- | **Properties**
propertyRenderParse :: TestTree
propertyRenderParse = QC.testProperty "renderStatements . parseStatements == id" $
  forAll genStatements $ \statements ->
    let rendered = Lib3.renderStatements statements
        parsed = Lib3.parseStatements rendered
    in parsed == Right (statements, "")

-- | **Property Tests**
propertyTests :: TestTree
propertyTests = testGroup "File System Property Tests"
  [ propertyRenderParse ]