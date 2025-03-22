module Main (main) where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import System.Directory
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (filterM)

import Summarizer (extractOutline, Options(..))

main :: IO ()
main = do
  -- Discover all Haskell test files
  inputFiles <- findHaskellFiles "test/golden/input"
  
  -- Create test cases for each file
  testCases <- mapM makeTestCase inputFiles
  
  -- Run the tests
  defaultMain $ testGroup "Golden tests" testCases

-- | Find all Haskell files in a directory
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      contents <- listDirectory dir
      let paths = map (dir </>) contents
      files <- filterM doesFileExist paths
      return $ filter (\f -> takeExtension f `elem` [".hs", ".lhs"]) files
    else return []
  -- Note: This function doesn't need to filter directories since it doesn't do recursive search,
  -- but the version in Summarizer.hs does filter out build tool directories

-- | Create a golden test case for a single file
makeTestCase :: FilePath -> IO TestTree
makeTestCase inputFile = do
  let 
    baseName = takeBaseName inputFile
    expectedFile = "test/golden/expected" </> baseName <.> "txt"
    
  -- Create options for running the summarizer
  let options = Options {
        optDirectory = ".",           -- Not used in tests
        optOnlySigs = True,           -- Show only signatures
        optOnlyTypes = False,         -- Show both types and signatures
        optShowLoc = False,           -- Don't show source locations
        optNoMinify = True,           -- Don't minify the output
        optPackage = Nothing          -- No package for tests
      }
  
  -- Return the test case
  return $ goldenVsStringDiff
    baseName                                -- Test name
    (\ref new -> ["diff", "-u", ref, new])  -- Diff command
    expectedFile                            -- Golden (expected) file
    (generateOutput options inputFile)      -- Action to generate output

-- | Generate the output for a Haskell file
generateOutput :: Options -> FilePath -> IO ByteString
generateOutput opts file = do
  -- Read the input file
  content <- readFile file
  
  -- Generate the summary
  let summary = extractOutline opts file content
  
  -- Convert to ByteString and return
  return $ BL.pack summary