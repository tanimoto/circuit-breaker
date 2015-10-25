{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------

module CircuitBreaker
  where

-------------------------------------------------------------------------------

import           CircuitBreaker.Options
import           CircuitBreaker.Types

import           Control.Applicative         (Applicative (..), (<$>))
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as Byte
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import           Options.Applicative         (execParser)

-------------------------------------------------------------------------------

app :: (MonadState Env m, MonadIO m) => Options -> m ()
app options = do
  liftIO $ print options

  env <- get
  liftIO $ putStrLn $ "Before: " ++ show env

  modify $ \s -> s { envCount = envCount s + 1 }

  newEnv <- get
  liftIO $ putStrLn $ "After: " ++ show newEnv

-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  options <- execParser parserInfo
  runApp $ app options
