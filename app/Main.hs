module Main where

import System.Directory (doesDirectoryExist, listDirectory, doesFileExist)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative

-- Import our summarizer module
import Summarizer (extractOutline, Options(..), summarizePackage)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "dir"
     <> short 'd'
     <> metavar "DIRECTORY"
     <> value "."
     <> help "Directory to analyze (default: current directory)" )
  <*> switch
      ( long "only-sigs"
     <> short 's'
     <> help "Only show signatures (no implementations)" )
  <*> switch
      ( long "only-types"
     <> short 't'
     <> help "Only show type definitions (data, newtype, type, class)" )
  <*> switch
      ( long "show-loc"
     <> short 'l'
     <> help "Show source code locations" )
  <*> switch
      ( long "no-minify"
     <> short 'n'
     <> help "Disable minification (minify is on by default)" )
  <*> optional (strOption
      ( long "package"
     <> short 'p'
     <> metavar "PACKAGE"
     <> help "Summarize the specified Haskell package" ))

-- | Main entry point: process Haskell files in the given directory
main :: IO ()
main = do
  options <- execParser opts
  hPutStrLn stderr "Haskell Project Summarizer"
  hPutStrLn stderr "=========================="
  
  -- Print the actual output header to stdout
  putStrLn "Haskell Project Summarizer"
  putStrLn "=========================="
  
  -- Check if we're summarizing a package or a directory
  case optPackage options of
    Just pkgName -> do
      hPutStrLn stderr $ "Summarizing package: " ++ pkgName
      putStrLn $ "Summarizing package: " ++ pkgName
      summary <- summarizePackage options pkgName
      putStrLn summary
    Nothing ->
      -- Process directory as before
      processDirectory options (optDirectory options)
  
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Generate a summary of Haskell source files or packages"
     <> header "hs-llm-summarizer - extract function signatures and comments" )

-- | Process a directory, finding all Haskell files (always recursive)
processDirectory :: Options -> FilePath -> IO ()
processDirectory opts dir = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      hPutStrLn stderr $ "Processing directory: " ++ dir
      contents <- listDirectory dir
      forM_ contents $ \item -> do
        let path = dir </> item
        isDir' <- doesDirectoryExist path
        if isDir'
          then processDirectory opts path  -- Always process subdirectories recursively
          else do
            isFile <- doesFileExist path
            when isFile $ processFile opts path
    else putStrLn $ "Not a directory: " ++ dir

-- | Process a single file if it's a Haskell file
processFile :: Options -> FilePath -> IO ()
processFile opts file =
  if takeExtension file `elem` [".hs", ".lhs"]
    then do
      hPutStrLn stderr $ "Processing file: " ++ file
      
      putStrLn $ "\nFile: " ++ file
      putStrLn $ replicate (length file + 6) '-'
      content <- TIO.readFile file
      let outline = extractOutline opts file (T.unpack content)
      putStrLn outline
    else return ()
