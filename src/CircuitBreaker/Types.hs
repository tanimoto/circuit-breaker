{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------

module CircuitBreaker.Types
  where

-------------------------------------------------------------------------------

import           Control.Applicative         (Applicative (..), (<$>))
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as Byte
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector

-------------------------------------------------------------------------------

data Env = Env
  { envCount :: Int
  } deriving (Eq, Show)

defaultEnv :: Env
defaultEnv = Env
  { envCount = 0
  }

-------------------------------------------------------------------------------

newtype App a = App { unApp :: StateT Env IO a }
  deriving (Functor, Applicative, Monad, MonadState Env, MonadIO)

runApp :: App a -> IO a
runApp = flip evalStateT defaultEnv . unApp

-------------------------------------------------------------------------------

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Eq, Show)


instance FromJSON Person where
  parseJSON (Object v) = Person <$>
                         v .: "name" <*>
                         v .: "age"

  parseJSON _ = fail "Failed to parse Person"
