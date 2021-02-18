{-# LANGUAGE LambdaCase #-}
module Main where

import System.Exit
import System.Environment
import Data.List

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import System.Console.Haskeline

import Language.Foma
import Kip.Parser
import Kip.AST
import Kip.Eval

import Paths_kip (version)
import Data.Version (showVersion)

data ReplState =
  ReplState
    { replCtx :: [Identifier]
    }
  deriving (Show)

main :: IO ()
main = 
  lookupEnv "TRMORPH" >>= \case
    Nothing -> die "TRMORPH ortam değişkeni mevcut değil."
    Just path -> do
      fsm <- fsmReadBinaryFile path
      
      -- TODO check if a file is passed in, then run the file instead of opening REPL

      -- Start REPL
      putStrLn $ "Kip " ++ showVersion version ++ "\n==================" 
      runInputT defaultSettings (loop fsm (ReplState []))
   where
    loop :: FSM -> ReplState -> InputT IO ()
    loop fsm rs = do
      minput <- getInputLine "Kip> "
      case minput of
          Nothing -> return ()
          Just ":çık" -> return ()
          Just ":quit" -> return ()
          Just input 
            | Just word <- stripPrefix ":name " input -> do 
                liftIO (ups fsm word) >>= \xs -> mapM_ outputStrLn xs
                loop fsm rs
            | Just word <- stripPrefix ":up " input -> do 
                liftIO (ups fsm word) >>= \xs -> mapM_ outputStrLn xs
                loop fsm rs
            | otherwise -> do 
                liftIO (print rs)
                let pst = MkParserState fsm (replCtx rs)
                liftIO (parseFromRepl pst input) >>= \case
                  Left err -> do
                    outputStrLn $ "Err: " ++ show err
                    loop fsm rs
                  Right (stmt, MkParserState _ pctx) -> do
                    outputStrLn $ show stmt
                    liftIO (runEvalM (replStmt stmt) emptyEvalState) >>= \case
                      Left evalErr -> outputStrLn $ "Eval err: " ++ show evalErr
                      Right (res, st) ->
                        return ()
                    loop fsm (rs {replCtx = pctx })
