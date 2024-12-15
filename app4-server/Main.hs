{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty
import qualified Lib3
import qualified Lib2
import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import GHC.Conc
import GHC.Base

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

parseCommandWithState :: String -> ExceptT String (State String) Lib3.Command
parseCommandWithState input = do
    lift $ Control.Monad.Trans.State.put input
    case Lib3.parseCommand input of
        Right (cmd, "") -> return $ cmd
        Right _ -> throwE $ "Parse Error: not whole string has been consumed"
        Left err  -> throwE $ "Parse Error: " ++ err


main :: IO ()
main = do
    state <- newTVarIO Lib2.emptyState
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    _ <- forkIO $ Lib3.storageOpLoop chan
    scotty 3000 $ do
        post "/upload" $ do
            b <- body
            let input = cs b
            let parser = parseCommandWithState input
            case runState (runExceptT parser) "" of
                (Right cmd, _) -> case cmd of
                    Lib3.SaveCommand -> do
                        result <- liftIO $ Lib3.stateTransition state Lib3.SaveCommand chan
                        respond result
                    Lib3.LoadCommand -> do
                        result <- liftIO $ Lib3.stateTransition state Lib3.LoadCommand chan
                        respond result
                    Lib3.StatementCommand b -> do
                        result <- liftIO $ Lib3.stateTransition state (Lib3.StatementCommand b) chan
                        respond result
                (Left err, _) -> text $ cs err


respond :: Either String (Maybe String) -> ActionM ()
respond result = case result of
    Right (Just str) -> text $ cs str
    Left str         -> text $ cs str
    _                -> text "Unknown response"

-- to test:
-- curl -X POST -d "LOAD" http://localhost:3000/upload