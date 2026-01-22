{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Non-interactive runner for Kip (playground/WASM).
module Main where

import Control.Applicative ((<|>))
import Control.Monad (when, unless, filterM)
import Control.Monad.IO.Class
import Control.Monad.Reader (runReaderT, ask)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashTable.IO as HT
import Data.Set (Set)
import qualified Data.Set as Set
import Options.Applicative hiding (ParseError)
import Paths_kip (getDataFileName)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die, exitSuccess)
import System.FilePath ((</>), takeDirectory)

import Language.Foma
import Kip.AST
import Kip.Eval hiding (Unknown)
import Kip.Parser
import Kip.Render
import Kip.Runner
import Kip.Codegen.JS (codegenProgram)
import Kip.TypeCheck

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

-- | Entry point for CLI modes.
main :: IO ()
main = do
  opts <- execParser (info (cliParser <**> helper) (fullDesc <> progDesc "Non-interactive runner for the Kip language"))
  let lang = optLang opts
  trmorphPath <- locateTrmorph lang
  libDir <- locateLibDir lang
  fsm <- fsmReadBinaryFile trmorphPath
  upsCache <- HT.new
  downsCache <- HT.new
  let renderCache = mkRenderCache upsCache downsCache
      moduleDirs = nub (libDir : optIncludeDirs opts)
      renderCtx = RenderCtx lang renderCache fsm upsCache downsCache
  (preludePst, preludeTC, preludeEval, preludeLoaded) <-
    runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
  case optMode opts of
    ModeExec -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFile) renderCtx
      _ <- runReaderT (runFiles False False False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
      exitSuccess
    ModeBuild -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFileOrDir) renderCtx
      buildTargets <- resolveBuildTargets (optFiles opts)
      let extraDirs = nub (map takeDirectory buildTargets)
          buildModuleDirs = nub (moduleDirs ++ extraDirs)
      (preludeBuildPst, preludeBuildTC, preludeBuildEval, preludeBuildLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) buildModuleDirs renderCache fsm upsCache downsCache) renderCtx
      _ <- runReaderT (runFiles False False True preludeBuildPst preludeBuildTC preludeBuildEval buildModuleDirs preludeBuildLoaded buildTargets) renderCtx
      exitSuccess
    ModeCodegen target -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFile) renderCtx
      case target of
        "js" -> do
          js <- runReaderT (emitJsFilesWithDeps moduleDirs preludePst preludeTC Set.empty (optFiles opts)) renderCtx
          TIO.putStrLn js
          exitSuccess
        _ ->
          die ("Unknown codegen target: " ++ T.unpack target)
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
      where
        parseLang :: String -> Either String Lang
        parseLang s =
          case s of
            "tr" -> Right LangTr
            "en" -> Right LangEn
            _ -> Left "LANG must be 'tr' or 'en'"

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

    locateTrmorph :: Lang -> IO FilePath
    locateTrmorph lang = do
      path <- locateDataFile "vendor/trmorph.fst"
      exists <- doesFileExist path
      if exists
        then return path
        else die . T.unpack $
          case lang of
            LangTr -> "vendor/trmorph.fst bulunamadı."
            LangEn -> "vendor/trmorph.fst not found."

    locateLibDir :: Lang -> IO FilePath
    locateLibDir lang = do
      path <- locateDataFile "lib/temel.kip"
      exists <- doesFileExist path
      if exists
        then return (takeDirectory path)
        else die . T.unpack $
          case lang of
            LangTr -> "lib/temel.kip bulunamadı."
            LangEn -> "lib/temel.kip not found."

    locateDataFile :: FilePath -> IO FilePath
    locateDataFile rel = do
      mEnv <- lookupEnv "KIP_DATADIR"
      cabalPath <- getDataFileName rel
      let envPaths = maybe [] (\base -> [base </> rel]) mEnv
          candidates = envPaths ++ [cabalPath, rel]
      found <- filterM doesFileExist candidates
      case found of
        p:_ -> return p
        [] -> return cabalPath

-- | Generate a single JS-like output for files and dependencies.
emitJsFilesWithDeps :: [FilePath] -> ParserState -> TCState -> Set FilePath -> [FilePath] -> RenderM Text
emitJsFilesWithDeps moduleDirs basePst baseTC _preludeLoaded files = do
  -- First load the prelude entry point to get all library definitions
  preludePath <- resolveModulePath moduleDirs ([], T.pack "giriş")
  (preludeStmts, pst', tcSt', loaded') <- emitJsFileWithDeps moduleDirs ([], basePst, baseTC, Set.empty) preludePath
  -- Then load the user files
  (stmts, _, _, _) <- foldM' (emitJsFileWithDeps moduleDirs) (preludeStmts, pst', tcSt', loaded') files
  return (codegenProgram stmts)

-- | Generate a single file and its dependencies.
emitJsFileWithDeps :: [FilePath] -> ([Stmt Ann], ParserState, TCState, Set FilePath) -> FilePath -> RenderM ([Stmt Ann], ParserState, TCState, Set FilePath)
emitJsFileWithDeps moduleDirs (acc, pst, tcSt, loaded) path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ do
    msg <- renderMsg (MsgFileNotFound path)
    liftIO (die (T.unpack msg))
  absPath <- liftIO (canonicalizePath path)
  if Set.member absPath loaded
    then return (acc, pst, tcSt, loaded)
    else do
      input <- liftIO (TIO.readFile path)
      liftIO (parseFromFile pst input) >>= \case
        Left err -> do
          msg <- renderMsg (MsgParseError err)
          liftIO (die (T.unpack msg))
        Right (fileStmts, pst') -> do
          let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
              tyMods = parserTyMods pst'
              loaded' = Set.insert absPath loaded
          -- First, process Load statements to get dependencies
          let loadStmts = [name | Load name <- fileStmts]
          (depStmts, pst'', tcSt', loaded'') <- foldM' (emitJsLoad moduleDirs paramTyCons tyMods) ([], pst', tcSt, loaded') loadStmts
          -- Now type-check the current file's statements
          liftIO (runTCM (registerForwardDecls fileStmts) tcSt') >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (_, tcStWithDecls) ->
              liftIO (runTCM (mapM tcStmt fileStmts) tcStWithDecls) >>= \case
                Left tcErr -> do
                  msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
                  liftIO (die (T.unpack msg))
                Right (typedStmts, tcSt'') ->
                  -- Filter out Load statements from typed output
                  let filteredStmts = filter (not . isLoadStmt) typedStmts
                  in return (acc ++ depStmts ++ filteredStmts, pst'', tcSt'', loaded'')

-- | Check if a statement is a Load statement.
isLoadStmt :: Stmt Ann -> Bool
isLoadStmt (Load _) = True
isLoadStmt _ = False

-- | Load a dependency for JS code generation.
emitJsLoad :: [FilePath] -> [Identifier] -> [(Identifier, [Identifier])] -> ([Stmt Ann], ParserState, TCState, Set FilePath) -> Identifier -> RenderM ([Stmt Ann], ParserState, TCState, Set FilePath)
emitJsLoad moduleDirs _paramTyCons _tyMods (acc, pst, tcSt, loaded) name = do
  path <- resolveModulePath moduleDirs name
  emitJsFileWithDeps moduleDirs (acc, pst, tcSt, loaded) path
