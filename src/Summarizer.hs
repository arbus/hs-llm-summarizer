module Summarizer 
  ( extractOutline
  , Options(..)
  , summarizePackage
  ) where

import Data.List (intercalate, sortBy, elemIndex)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import System.Process (readProcess)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStrLn, stderr)
import Control.Monad (filterM, when)
import Control.Exception (catch, SomeException)
import qualified System.Environment

-- Import haskell-src-exts for parsing
import Language.Haskell.Exts

-- | Command line options
data Options = Options
  { optDirectory :: FilePath         -- ^ Directory to analyze
  , optOnlySigs  :: Bool             -- ^ Whether to show only signatures (no implementations)
  , optOnlyTypes :: Bool             -- ^ Whether to show only type definitions
  , optShowLoc   :: Bool             -- ^ Whether to show source locations
  , optNoMinify  :: Bool             -- ^ Whether to disable minification (off = minify)
  , optPackage   :: Maybe String     -- ^ Optional package name to analyze
  }

-- | Extract function names, type signatures, and comments from Haskell code
-- using haskell-src-exts for robust parsing
extractOutline :: Options -> FilePath -> String -> String
extractOutline opts file content = 
  let 
    -- Preprocess content based on the file
    processedContent = preprocess file content
    
    moduleHeader = "-- Module: " ++ file ++ "\n\n"
    -- Configure parser with ALL extensions enabled to handle most Haskell files
    -- regardless of whether they are explicitly enabled in the file or project
    mode = defaultParseMode { 
      extensions = map EnableExtension allExtensions, -- Enable ALL known extensions
      ignoreLanguagePragmas = False,  -- Parse language pragmas in the file
      ignoreLinePragmas = True,       -- Ignore line pragmas
      fixities = Nothing              -- Use default fixities
    }
    
    -- Get all positive extensions by enumerating KnownExtension
    allExtensions = filter isPositiveExtension [minBound..maxBound]
    
    -- Filter out the "no" extensions and only keep the positive ones
    isPositiveExtension ext = "No" `notElem` (words $ show ext)
    
    parseResult = parseModuleWithComments mode processedContent
    result = case parseResult of
      ParseOk (m, comments) -> 
        moduleHeader ++ formatModule opts m comments
      ParseFailed srcLoc message ->
        moduleHeader ++ "Parse error at " ++ show srcLoc ++ ": " ++ message ++ 
        "\n\nNote: The parser may have issues with some language extensions. " ++
        "Extracted information may be incomplete."
  in 
    -- Apply minification if requested
    applyMinify opts result
    
-- | Preprocess the file content based on the file path
preprocess :: FilePath -> String -> String
preprocess _ content = processAllImports content

-- | Process all imports in the file, including post-qualified imports
processAllImports :: String -> String
processAllImports content =
  let
    -- Add language pragmas
    pragmas = [
      "{-# LANGUAGE GADTs #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "{-# LANGUAGE DataKinds #-}",
      "{-# LANGUAGE PolyKinds #-}",
      "{-# LANGUAGE KindSignatures #-}",
      "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE MultiParamTypeClasses #-}",
      "{-# LANGUAGE FunctionalDependencies #-}",
      "{-# LANGUAGE TypeOperators #-}",
      "{-# LANGUAGE DerivingStrategies #-}",
      "{-# LANGUAGE DeriveAnyClass #-}",
      "{-# LANGUAGE NamedFieldPuns #-}",
      "{-# LANGUAGE RecordWildCards #-}",
      "{-# LANGUAGE ExplicitNamespaces #-}",
      "{-# LANGUAGE PackageImports #-}"
      ]
    
    -- Process each line
    processLine line
      -- Skip already properly formatted qualified imports
      | "import qualified " `isPrefixOf` line = line
      
      -- Handle post-qualified imports like "import Data.Text qualified as T" 
      -- or "import PI.Db.User qualified as Db"
      | "import " `isPrefixOf` line && " qualified" `isInfixOf` line =
          let
            -- Split the line into tokens
            tokens = words line
            
            -- Find the index of "qualified"
            qualifiedIndex = fromMaybe 0 $ elemIndex "qualified" tokens
            
            -- Extract the module name (everything between "import" and "qualified")
            moduleTokens = take (qualifiedIndex - 1) (tail tokens) -- Skip "import"
            moduleName = unwords moduleTokens
            
            -- Check if there's an "as" qualifier
            hasAs = (qualifiedIndex + 1 < length tokens) && 
                   (tokens !! (qualifiedIndex + 1) == "as")
            
            -- Extract the alias if present
            alias = if hasAs && (qualifiedIndex + 2 < length tokens)
                    then tokens !! (qualifiedIndex + 2)
                    else ""
            
            -- Build the new import statement
            newImport = 
              if hasAs && not (null alias)
              then "import qualified " ++ moduleName ++ " as " ++ alias
              else "import qualified " ++ moduleName
          in
            newImport
          
      -- Pass through other lines
      | otherwise = line
      
    -- Process all lines
    contentLines = lines content
    processedLines = map processLine contentLines
    
    -- Combine pragmas and processed content
    result = pragmas ++ processedLines
  in
    unlines result
    

-- | Format a parsed module into a readable outline
formatModule :: Options -> Module SrcSpanInfo -> [Comment] -> String
formatModule opts m comments =
  case m of
    Module _ _ _ _ decls -> 
      let 
        -- Sort by source line number
        sortedDecls = sortBy (compare `on` (srcSpanStartLine . srcInfoSpan . ann)) decls
        sortedComments = sortBy (compare `on` (\(Comment _ srcSpan _) -> srcSpanStartLine srcSpan)) comments
        declsWithComments = associateComments sortedDecls sortedComments
      in
        intercalate "\n\n" $ map (formatDecl opts) declsWithComments
    _ -> "Unsupported module type"

-- | Associate comments with their closest declarations
associateComments :: [Decl SrcSpanInfo] -> [Comment] -> [(Decl SrcSpanInfo, [Comment])]
associateComments decls comments = 
  map (\d -> (d, commentsForDecl d comments)) decls
  where
    commentsForDecl decl = filter (belongsToDecl decl)
    
    belongsToDecl :: Decl SrcSpanInfo -> Comment -> Bool
    belongsToDecl decl (Comment _ commentSpan _) =
      let declSpan = srcInfoSpan (ann decl)
          commentLine = srcSpanEndLine commentSpan
          declLine = srcSpanStartLine declSpan
      in commentLine < declLine && declLine - commentLine <= 2

-- | Format a declaration with its comments
formatDecl :: Options -> (Decl SrcSpanInfo, [Comment]) -> String
formatDecl opts (decl, comments) =
  let 
    -- Format attached comments
    formattedComments = if null comments 
                        then "" 
                        else unlines (map commentText comments) ++ "\n"
    
    -- Add source location if requested
    locInfo = if optShowLoc opts
              then formatSourceLoc (ann decl) ++ "\n"
              else ""
                        
    -- Format the declaration based on its pretty-printed representation
    declStr = prettyPrint decl
    
    -- Check if it's a type-related declaration
    isTypeDecl = any (\prefix -> prefix `isPrefixOf` declStr) 
                  ["data ", "newtype ", "type ", "class "]
    
    -- Check if it's a type signature
    isTypeSig = " :: " `isInfixOf` declStr && not (" = " `isInfixOf` declStr)
    
    -- Check if it's a function/value implementation 
    isImpl = " = " `isInfixOf` declStr && not (any (`isPrefixOf` declStr) 
                     ["data ", "newtype ", "type ", "class "])
                     
    -- Apply filtering based on options
    formattedDecl = 
      if optOnlyTypes opts && not isTypeDecl then
        -- Skip if only showing types and this isn't a type declaration
        ""
      else if optOnlySigs opts && isImpl then
        -- Skip implementations if only showing signatures
        ""
      else if optOnlyTypes opts && not (optOnlySigs opts) && isTypeSig then
        -- Skip signatures if only showing types (but not signatures)
        ""
      else if "type " `isPrefixOf` declStr && " = " `isInfixOf` declStr then
        -- It's a type declaration
        declStr
      else if "data " `isPrefixOf` declStr then
        -- It's a data declaration
        declStr
      else if "newtype " `isPrefixOf` declStr then
        -- It's a newtype declaration
        declStr
      else if "class " `isPrefixOf` declStr then
        -- It's a class declaration
        declStr
      else if "instance " `isPrefixOf` declStr then
        -- It's an instance declaration
        declStr
      else if isTypeSig then
        -- It's a type signature
        declStr
      else if isImpl then
        -- It's a function or pattern binding
        takeWhile (/= '=') declStr ++ "= [implementation]"
      else
        -- Other declarations
        "-- Other: " ++ take 60 declStr ++
        if length declStr > 60 then "..." else ""
  in
    if null formattedDecl 
    then "" -- Skip empty declarations
    else formattedComments ++ locInfo ++ formattedDecl
    
-- | Format source location information
formatSourceLoc :: SrcSpanInfo -> String
formatSourceLoc srcInfo =
  let srcSpan = srcInfoSpan srcInfo
      srcStartLine = srcSpanStartLine srcSpan
      srcStartCol = srcSpanStartColumn srcSpan
      srcEndLine = srcSpanEndLine srcSpan
      srcEndCol = srcSpanEndColumn srcSpan
  in "-- " ++ show srcStartLine ++ ":" ++ show srcStartCol ++ 
     "-" ++ show srcEndLine ++ ":" ++ show srcEndCol

-- | Extract comment text
commentText :: Comment -> String
commentText (Comment _ _ text) = text

-- | Check if a string is a prefix of another string
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Check if a string contains a substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = 
  any (isPrefixOf needle) (tails haystack)
  
-- | Get all possible tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

-- | Minify Haskell code by removing extra whitespace
minifyCode :: String -> String
minifyCode code = 
  -- Strip extra whitespace
  unwords . 
  -- Filter out comment-only lines and empty lines
  filter (not . isEmptyOrComment) . words $ 
  -- Replace common patterns for better minification
  foldl applyReplacement code replacementPatterns
  where
    -- Apply a single replacement pattern
    applyReplacement text (from, to) = replace from to text
    
    -- Pattern replacements for Haskell code
    replacementPatterns = 
      [ (":: ", "::"), (" ::", "::"), (" = ", "="), 
        ("= ", "="), (" =", "="), ("( ", "("), (" )", ")"),
        (" {", "{"), ("{ ", "{"), (" }", "}"), ("} ", "}") ]
    
    -- Basic string replacement
    replace from to = go where
      go [] = []
      go s@(x:xs)
        | from `isPrefixOf` s = to ++ go (drop (length from) s)
        | otherwise = x : go xs
        
    -- Check if a string is only comments or empty
    isEmptyOrComment str = null str || "--" `isPrefixOf` str
      
-- | Apply minification if not disabled
applyMinify :: Options -> String -> String
applyMinify opts text =
  if optNoMinify opts
  then text  -- Not minified if no-minify is specified
  else minifyCode text  -- Minified by default

-- | Summarize a Haskell package by name
-- This function allows the user to specify a package name
-- and will attempt to summarize its source files.
summarizePackage :: Options -> String -> IO String
summarizePackage opts packageName = do
  -- First try to download with cabal get for best results
  hPutStrLn stderr $ "Attempting to download package " ++ packageName ++ " using 'cabal get'..."
  downloadedPath <- downloadPackage packageName
  case downloadedPath of
    Just path -> do
      hPutStrLn stderr $ "Downloaded package source to: " ++ path
      exists <- doesDirectoryExist path
      
      if not exists then
        return $ "Error: Package directory for " ++ packageName ++ " at " ++ path ++ " does not exist.\n" ++
                "Try downloading it manually with 'cabal get " ++ packageName ++ "' to a directory of your choice and then use:\n" ++
                "  hs-llm-summarizer --dir <path-to-downloaded-package>"
      else do
        -- Check if there's a src directory and use it directly if it exists
        let srcPath = path </> "src"
        srcExists <- doesDirectoryExist srcPath
        
        if srcExists then do
          hPutStrLn stderr $ "Found src directory, using it directly: " ++ srcPath
          processPkgDirectory opts packageName srcPath
        else
          processPkgDirectory opts packageName path
          
    Nothing -> do
      -- If download fails, try to find in local GHC packages, but warn that
      -- these might not have source files
      hPutStrLn stderr $ "Couldn't download package. Checking local GHC databases..."
      hPutStrLn stderr $ "Note: Installed packages often don't include source files (.hs), only compiled modules."
      pkgInfo <- findPackagePath packageName
      case pkgInfo of
        Just pkgPath -> do
          hPutStrLn stderr $ "Found installed package at: " ++ pkgPath
          exists <- doesDirectoryExist pkgPath
          
          if not exists then
            return $ "Error: Found package " ++ packageName ++ " in database, but directory " ++ pkgPath ++ " does not exist."
          else do
            result <- processPkgDirectory opts packageName pkgPath
            
            -- Check if we found any files
            if "No Haskell source files found" `isPrefixOf` result then
              return $ "No Haskell source files (.hs) found for installed package " ++ packageName ++ " at " ++ pkgPath ++ ".\n" ++
                      "This is expected for installed packages, which typically only contain compiled modules.\n" ++
                      "To get source files, please download the package with:\n" ++
                      "  cabal get " ++ packageName ++ "\n" ++
                      "And then run:\n" ++
                      "  hs-llm-summarizer --dir " ++ packageName ++ "-<version>/src"
            else
              return result
              
        Nothing -> 
          return $ "Error: Package " ++ packageName ++ " could not be downloaded or found in local databases.\n" ++
                  "Try downloading it manually with 'cabal get " ++ packageName ++ "' to a directory of your choice and then use:\n" ++
                  "  hs-llm-summarizer --dir <path-to-downloaded-package>"

-- | Download a package using cabal get
downloadPackage :: String -> IO (Maybe FilePath) 
downloadPackage packageName = do
  -- Always use temp directory for downloads to avoid cluttering the user's working directory
  tmpDir <- (</> ("hackage-" ++ packageName)) <$> System.Directory.getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  
  hPutStrLn stderr $ "Downloading package " ++ packageName ++ " to temporary directory: " ++ tmpDir
  
  -- Download the package to the temp directory
  result <- catchIO 
    (do
      -- Try to download the package with cabal get
      _ <- readProcess "cabal" ["get", packageName, "-d", tmpDir] ""
      
      -- Find the actual package directory (which includes version)
      tmpDirExists <- doesDirectoryExist tmpDir
      if not tmpDirExists then do
        hPutStrLn stderr $ "Error: Temp directory doesn't exist after download: " ++ tmpDir
        return Nothing
      else do
        dirs <- listDirectory tmpDir
        let pkgDirs = filter (packageName `isPrefixOf`) dirs
        
        if null pkgDirs
          then do
            hPutStrLn stderr "Error: No matching package directory found in temp dir"
            return Nothing
          else do
            let packageDir = tmpDir </> head pkgDirs
            hPutStrLn stderr $ "Found package directory: " ++ packageDir
            return $ Just packageDir)
    (\e -> do
      hPutStrLn stderr $ "Cabal get error: " ++ show e
      return Nothing)
      
  -- Return the package directory path
  return result

-- | Process a package directory to find and summarize Haskell files
processPkgDirectory :: Options -> String -> FilePath -> IO String
processPkgDirectory opts packageName pkgPath = do
  -- Find all Haskell source files in the package directory
  hPutStrLn stderr $ "Finding Haskell files in: " ++ pkgPath
  sourceFiles <- findHaskellFiles pkgPath
  
  if null sourceFiles
    then do 
      hPutStrLn stderr $ "No Haskell source files found for package " ++ packageName
      return $ "No Haskell source files found for package " ++ packageName ++ " at " ++ pkgPath
    else do
      hPutStrLn stderr $ "Found " ++ show (length sourceFiles) ++ " Haskell files to process"
      -- Process each source file and combine the results
      results <- mapM (summarizePackageFile opts) sourceFiles
      hPutStrLn stderr $ "Successfully processed all files for package " ++ packageName
      return $ "Package: " ++ packageName ++ "\n\n" ++
              intercalate "\n\n" results


-- | Find the path to a package's source files
findPackagePath :: String -> IO (Maybe FilePath)
findPackagePath packageName = do
  -- First try GHC's package database
  result <- findPathWithGhcPkg packageName
  case result of
    Just path -> return $ Just path
    Nothing -> do
      -- Then try cabal's store in ~/.cabal
      findPathInCabalStore packageName

-- | Find a package path using ghc-pkg
findPathWithGhcPkg :: String -> IO (Maybe FilePath)
findPathWithGhcPkg packageName = do
  -- Try to get the package's library directory from ghc-pkg
  hPutStrLn stderr $ "Checking if package " ++ packageName ++ " is installed..."
  libDirOutput <- catchIO 
    (readProcess "ghc-pkg" ["field", packageName, "library-dirs"] "")
    (\e -> do
      hPutStrLn stderr $ "ghc-pkg error: " ++ show e
      return "")
      
  if "library-dirs:" `isPrefixOf` libDirOutput
    then do
      let libDir = drop 14 $ filter (/= '\n') libDirOutput
      hPutStrLn stderr $ "Found package library directory: " ++ libDir
      return $ Just libDir
    else do
      hPutStrLn stderr $ "Package " ++ packageName ++ " not found in ghc-pkg database."
      return Nothing

-- | Find a package path in the cabal store
findPathInCabalStore :: String -> IO (Maybe FilePath)
findPathInCabalStore packageName = do
  homeDir <- getEnvVar "HOME"
  -- Check common GHC versions
  let baseDirs = [homeDir </> ".cabal/store/ghc-9.4.8",
                 homeDir </> ".cabal/store/ghc-9.4.4",
                 homeDir </> ".cabal/store/ghc-9.2.8"]
  
  hPutStrLn stderr $ "Checking for package in cabal store..."
  
  -- Try to find the package in one of the directories
  results <- mapM (findPackageInDir packageName) baseDirs
  let result = findFirst results
  case result of
    Just path -> hPutStrLn stderr $ "Found package in cabal store: " ++ path
    Nothing -> hPutStrLn stderr $ "Package not found in cabal store."
  return result
  where
    findFirst [] = Nothing
    findFirst (Just p:_) = Just p
    findFirst (Nothing:rest) = findFirst rest

-- | Find a package in a specific cabal store directory
findPackageInDir :: String -> FilePath -> IO (Maybe FilePath)
findPackageInDir packageName baseDir = do
  dirExists <- doesDirectoryExist baseDir
  if not dirExists
    then do
      hPutStrLn stderr $ "Directory does not exist: " ++ baseDir
      return Nothing
    else do
      -- List all directories and find one that starts with the
      -- vowel-removed version of the package name
      let packagePrefix = removeVowels packageName
      hPutStrLn stderr $ "Looking for prefix '" ++ packagePrefix ++ "' in " ++ baseDir
      entries <- listDirectory baseDir
      let matchingEntries = filter (packagePrefix `isPrefixOf`) entries
      if null matchingEntries
        then do
          hPutStrLn stderr $ "No matching entries found in " ++ baseDir
          return Nothing
        else do
          let pkgDir = baseDir </> head matchingEntries
          hPutStrLn stderr $ "Found matching entry: " ++ pkgDir
          return $ Just pkgDir

-- | Remove vowels from a string (to match how cabal shortens package names)
removeVowels :: String -> String
removeVowels = filter (`notElem` "aeiouAEIOU")

-- | Find all Haskell source files in a directory and its subdirectories
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then do
      hPutStrLn stderr $ "Directory does not exist: " ++ dir
      return []
    else do
      hPutStrLn stderr $ "Searching directory: " ++ dir
      entries <- listDirectory dir
      
      let fullPaths = map (dir </>) entries
      
      -- Separate files and directories
      files <- filterM doesFileExist fullPaths
      dirs <- filterM doesDirectoryExist fullPaths
      
      -- Get Haskell files from the current directory
      let haskellFiles = filter (\f -> takeExtension f `elem` [".hs", ".lhs"]) files
      when (not $ null haskellFiles) $
        hPutStrLn stderr $ "Found " ++ show (length haskellFiles) ++ " Haskell files in " ++ dir
      
      -- Recursively search subdirectories
      subDirFiles <- concat <$> mapM findHaskellFiles dirs
      
      -- Combine results
      return $ haskellFiles ++ subDirFiles

-- | Summarize a single Haskell file from a package
summarizePackageFile :: Options -> FilePath -> IO String
summarizePackageFile opts file = do
  hPutStrLn stderr $ "Processing file: " ++ file
  content <- readFile file
  return $ "File: " ++ file ++ "\n" ++
           replicate (length file + 6) '-' ++ "\n" ++
           extractOutline opts file content

-- | Get environment variable safely
getEnvVar :: String -> IO String
getEnvVar varName = catchIO (System.Environment.getEnv varName) (const $ return "")

-- | Catch IO exceptions and handle them with a default value
catchIO :: IO a -> (SomeException -> IO a) -> IO a
catchIO = catch