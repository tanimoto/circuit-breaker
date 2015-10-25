{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------

module CircuitBreaker.Options
  ( Options (..)
  , defaultOptions
  , parserOptions
  , parserInfo
  ) where

-------------------------------------------------------------------------------

import           Options.Applicative

-------------------------------------------------------------------------------

data Options = Options
  { optionsInput :: FilePath
  , optionsOutput :: FilePath
  } deriving (Eq, Show)


defaultOptions :: Options
defaultOptions = Options
  { optionsInput = "input"
  , optionsOutput = "output"
  }

-------------------------------------------------------------------------------

parserInfo :: ParserInfo Options
parserInfo =
  info (helper <*> parserOptions)
    ( fullDesc
    <> progDesc ""
    <> header "circuit-breaker"
    )


parserOptions :: Parser Options
parserOptions = Options
  <$> strOption
      ( long "input"
      <> metavar "FILE"
      <> help "Input file"
      )
  <*> strOption
      ( long "output"
      <> metavar "FILE"
      <> help "Output file"
      )
