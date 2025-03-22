module Main where

import Control.Monad (forM_, unless, when)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
-- Import our summarizer module
import Summarizer (Options (..), extractOutline, summarizePackage)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, takeFileName, (</>))
import System.IO ()

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> value "."
          <> help "Directory to analyze (default: current directory)"
      )
    <*> switch
      ( long "only-sigs"
          <> short 's'
          <> help "Only show signatures (no implementations)"
      )
    <*> switch
      ( long "only-types"
          <> short 't'
          <> help "Only show type definitions (data, newtype, type, class)"
      )
    <*> switch
      ( long "show-loc"
          <> short 'l'
          <> help "Show source code locations"
      )
    <*> switch
      ( long "no-minify"
          <> short 'n'
          <> help "Disable minification (minify is on by default)"
      )
    <*> optional
      ( strOption
          ( long "package"
              <> short 'p'
              <> metavar "PACKAGE"
              <> help "Summarize the specified Haskell package"
          )
      )

-- | Main entry point: process Haskell files in the given directory
main :: IO ()
main = do
  options <- execParser opts
  -- Check if we're summarizing a package or a directory
  case optPackage options of
    Just pkgName -> do
      -- Skip debug output
      putStrLn $ "Summarizing package: " ++ pkgName
      summary <- summarizePackage options pkgName
      putStrLn summary
    Nothing ->
      -- Process directory as before
      processDirectory options (optDirectory options)
  where
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Generate a summary of Haskell source files or packages"
            <> header "hs-llm-summarizer - extract function signatures and comments"
        )

-- | Process a directory, finding all Haskell files (always recursive)
processDirectory :: Options -> FilePath -> IO ()
processDirectory opts dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      -- Skip debug output
      contents <- listDirectory dir
      forM_ contents $ \item -> do
        let path = dir </> item
        isDir' <- doesDirectoryExist path
        if isDir'
          then do
            -- Skip build tool and version control directories
            let dirName = takeFileName path
            let shouldSkip = dirName `elem` ["dist", "dist-newstyle", ".cabal", ".stack-work", ".git"]
            unless shouldSkip $
              processDirectory opts path -- Process non-ignored subdirectories recursively
          else do
            isFile <- doesFileExist path
            when isFile $ processFile opts path
    else putStrLn $ "Not a directory: " ++ dir

-- | Process a single file if it's a Haskell file
processFile :: Options -> FilePath -> IO ()
processFile opts file =
  if takeExtension file `elem` [".hs", ".lhs"]
    then do
      -- Skip debug output

      putStrLn $ "\nFile: " ++ file
      putStrLn $ replicate (length file + 6) '-'
      content <- TIO.readFile file
      let outline = extractOutline opts file (T.unpack content)
      putStrLn outline
    else return ()
