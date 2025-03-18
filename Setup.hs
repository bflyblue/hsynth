import Control.Monad
import Data.List (isPrefixOf)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Types.ForeignLib
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity
import System.Directory
import System.FilePath

-- | Main entry point for the custom setup
main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = \args buildFlags packageDesc localBuildInfo -> do
          -- Run the standard post-build actions first
          postBuild simpleUserHooks args buildFlags packageDesc localBuildInfo

          -- Then run our custom actions
          let verbosity = fromFlag (buildVerbosity buildFlags)
          processClapBundles verbosity packageDesc localBuildInfo
      }

-- | Create the CLAP bundle structure in the project root
processClapBundles :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
processClapBundles verbosity pkgDesc localBuildInfo = do
  -- Get all foreign libraries from the package description
  let libs = foreignLibs pkgDesc

  -- Process each foreign library
  forM_ libs $ \lib -> do
    let libName = unUnqualComponentName $ foreignLibName lib

    -- Only process libraries ending with "-clap"
    when (endsWithStr "-clap" libName) $ do
      putStrLn $ "Creating CLAP file for: " ++ libName

      -- For new-style builds, DLLs are found in the dist-newstyle directory
      -- We'll search for them recursively
      currentDir <- getCurrentDirectory
      let distDir = currentDir </> "dist-newstyle"
          clapFileName = libName ++ ".clap"
          clapFilePath = currentDir </> clapFileName

      -- Search for the DLL file - this handles different GHC versions and build configurations
      dllFiles <- findDLLFiles distDir (libName ++ ".dll")

      case dllFiles of
        [] -> putStrLn $ "Warning: No DLL found for " ++ libName
        (dllPath : _) -> do
          -- Simply copy the DLL to a file with .clap extension
          putStrLn $ "Copying " ++ dllPath ++ " to " ++ clapFilePath
          copyFile dllPath clapFilePath

-- | Recursively search for DLL files matching a pattern
findDLLFiles :: FilePath -> String -> IO [FilePath]
findDLLFiles dir pattern = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- listDirectory dir
      let fullPaths = map (dir </>) contents

      -- Check each item
      files <- filterM doesFileExist fullPaths
      dirs <- filterM doesDirectoryExist fullPaths

      -- Find matching files
      let matchingFiles = filter (\f -> takeFileName f == pattern) files

      -- Recursively search subdirectories
      subDirMatches <- concat <$> mapM (\d -> findDLLFiles d pattern) dirs

      -- Combine results
      return $ matchingFiles ++ subDirMatches

-- | Helper function to check if a string ends with a suffix
endsWithStr :: String -> String -> Bool
endsWithStr suffix str = reverse suffix `isPrefixOf` reverse str