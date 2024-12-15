{-# LANGUAGE DeriveFunctor #-}
module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString (ByteString)
import Network.Wreq
import Data.String.Conversions
import Control.Lens

data FileSystemAlgebra next = 
      Load (() -> next) 
    | Save (() -> next) 
    | Batch [String] (() -> next)
    deriving Functor

type FileSystemDomain = Free FileSystemAlgebra

load :: FileSystemDomain ()
load = liftF $ Load id

save :: FileSystemDomain ()
save = liftF $ Save id

batch :: [String] -> FileSystemDomain ()
batch queries = liftF $ Batch queries id

-- ** Interpreters ** --
-- 1. Single Request Interpreter --
interpretSingle :: FileSystemDomain a -> IO a
interpretSingle (Pure a) = return a
interpretSingle (Free cmd) = do
    case cmd of
        Load next -> sendRequest "load" >> interpretSingle (next ())
        Save next -> sendRequest "save" >> interpretSingle (next ())
        Batch cmds next -> do
            putStrLn "Sending batch commands..."
            let body = cs $ "BEGIN\n" ++ unlines cmds ++ "END" :: ByteString
            resp <- post "http://localhost:3000/upload" (cs body :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())

sendRequest :: String -> IO ()
sendRequest command = do
    putStrLn $ "Sending command: " ++ command
    resp <- post "http://localhost:3000/upload" (cs command :: ByteString)
    putStrLn $ "Response: " ++ cs (resp ^. responseBody)

-- 2. Batch Interpreter
interpretBatch :: FileSystemDomain a -> IO a
interpretBatch = go []
  where
    go :: [String] -> FileSystemDomain a -> IO a
    go acc (Pure a) = do
        flushBatch acc 
        return a
    go acc (Free cmd) =
        case cmd of
            Load next -> do
                flushBatch acc
                putStrLn "Sending load command..."
                resp <- post "http://localhost:3000/upload" (cs "load" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Save next -> do
                flushBatch acc
                putStrLn "Sending save command..."
                resp <- post "http://localhost:3000/upload" (cs "save" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Batch cmds next -> do
                go (acc ++ cmds) (next ())

    flushBatch :: [String] -> IO ()
    flushBatch [] = return ()
    flushBatch cmds = do
        putStrLn "Sending accumulated batch commands..."
        let body = cs $ ("BEGIN\n" ++ unlines cmds ++ "END") :: ByteString
        resp <- post "http://localhost:3000/upload" (cs body :: ByteString)
        putStrLn $ "Response: " ++ cs (resp ^. responseBody)

-- 3. Memory Interpreter
interpretInMemory :: FileSystemDomain a -> IO a
interpretInMemory (Pure a) = return a
interpretInMemory (Free cmd) = do
   case cmd of
       Load next -> do
        putStrLn "Simulating load in memory..."
        interpretInMemory (next ())
       Save next -> do
           putStrLn "Simulating save in memory..."
           interpretInMemory (next ())
       Batch cmds next -> do
           putStrLn "Simulating batch in memory..."
           interpretInMemory (next ())

-- ** Example Program ** --
exampleProgram :: FileSystemDomain ()
exampleProgram = do
    load
    batch [ "create directory directoryA","create file fileA 100 directoryA"]
    save
    batch ["create directory directoryB" , "create file fileB 200 directoryB","change file size directoryA fileA 500"]
    save
    batch ["delete directory directoryB", "create directory directoryC"]
    batch ["create file fileC 250 directoryC", "create file fileD 666 directoryC"]
    save

main :: IO ()
main = do
    putStrLn "Running with Single Command Interpreter:"
    interpretSingle exampleProgram

    putStrLn "Running with Batched Command Interpreter:"
    -- interpretBatch exampleProgram

    putStrLn "Running with In-Memory Interpreter:"
    -- interpretInMemory exampleProgram
