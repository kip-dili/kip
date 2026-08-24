{-# LANGUAGE OverloadedStrings #-}
{- |
Non-interactive playground runner.

This executable remains a thin CLI adapter around 'runPlaygroundRequest' so
the actual execution semantics are shared with the reactor entrypoint.
-}
module Main where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative hiding (ParseError)
import Kip.Runner
import Kip.Playground

-- | Supported CLI modes.
data CliMode
  = ModeExec
  | ModeBuild
  | ModeCodegen Text
  deriving (Eq, Show)

-- | Parsed CLI options.
data CliOptions =
  CliOptions
    { optMode :: CliMode
    , optFiles :: [FilePath]
    , optIncludeDirs :: [FilePath]
    , optLang :: Lang
    , optNoPrelude :: Bool
    }

-- | Parse CLI arguments and run one isolated playground request.
main :: IO ()
main = do
  opts <- execParser (info (cliParser <**> helper) (fullDesc <> progDesc "Non-interactive runner for the Kip language"))
  result <- runPlaygroundRequest (toPlaygroundRequest opts)
  case result of
    PlaygroundNoOutput -> return ()
    PlaygroundTextOutput txt -> TIO.putStrLn txt
  where
    cliParser :: Parser CliOptions
    cliParser =
        CliOptions
          <$> modeParser
          <*> many (strArgument (metavar "FILE..."))
          <*> many (strOption (short 'I' <> metavar "DIR" <> help "Additional module directory (used by `temeli yükle` etc.)"))
          <*> langParser
          <*> switch (long "no-prelude" <> help "Disable automatic loading of lib/giriş.kip")

    langParser :: Parser Lang
    langParser =
      option (eitherReader parseLang)
        ( long "lang"
        <> metavar "LANG"
        <> value LangTr
        <> help "Language for diagnostics (tr|en)"
        )

    modeParser :: Parser CliMode
    modeParser =
      flag' ModeExec (long "exec" <> help "Run files and exit")
        <|> flag' ModeBuild (long "build" <> help "Build cache files for the given files or directories")
        <|> (ModeCodegen . T.pack <$> strOption
              ( long "codegen"
              <> metavar "TARGET"
              <> help "Codegen target for the given files (e.g. js)"
              ))
        <|> pure ModeExec

toPlaygroundRequest :: CliOptions -> PlaygroundRequest
toPlaygroundRequest opts =
  PlaygroundRequest
    { prMode = toPlaygroundMode (optMode opts)
    , prFiles = optFiles opts
    , prIncludeDirs = optIncludeDirs opts
    , prLang = optLang opts
    , prNoPrelude = optNoPrelude opts
    }

toPlaygroundMode :: CliMode -> PlaygroundMode
toPlaygroundMode mode =
  case mode of
    ModeExec -> PlaygroundExec
    ModeBuild -> PlaygroundBuild
    ModeCodegen target -> PlaygroundCodegen target
