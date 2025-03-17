{-# LANGUAGE RecordWildCards #-}

module Main where

import Audio.Clap.Entry
import Audio.Clap.Version
import Export (clapEntry)
import Foreign
import Foreign.C.String
import Prelude hiding (init)

main :: IO ()
main = do
  putStrLn "HSynth CLAP Plugin Test"
  putStrLn "-----------------------"

  putStrLn $ "CLAP Version: " ++ showVersion clapCurrentVersion
  putStrLn $ "Major: " ++ show clapVersionMajor
  putStrLn $ "Minor: " ++ show clapVersionMinor
  putStrLn $ "Revision: " ++ show clapVersionRevision
  putStrLn $ "Compatible: " ++ show (clapVersionIsCompatible clapCurrentVersion)

  -- Test the plugin's entry point
  putStrLn "\nTesting plugin entry point implementation:"

  -- Get the entry point from our Export module
  putStrLn "Reading entry point structure..."
  entry <- peek clapEntry

  -- Test path for init
  testPath <- newCString "/path/to/test/plugin"

  -- Call init multiple times to test reference counting
  putStrLn "\nCalling plugin init 3 times:"
  initResult1 <- callInitFunction entry testPath
  initResult2 <- callInitFunction entry testPath
  initResult3 <- callInitFunction entry testPath

  putStrLn $ "Init results: " ++ show [initResult1, initResult2, initResult3]

  -- Call deinit to decrement counter but not free resources
  putStrLn "\nCalling plugin deinit 2 times (should decrement counter):"
  callDeinitFunction entry
  callDeinitFunction entry

  -- Call init again to test incrementing after partial deinit
  putStrLn "\nCalling plugin init again:"
  initResult4 <- callInitFunction entry testPath

  -- Finally, call deinit the last 2 times to free resources
  putStrLn "\nCalling plugin deinit the final 2 times (should clean up):"
  callDeinitFunction entry
  callDeinitFunction entry

  -- Clean up
  free testPath

  putStrLn "\nDone."

-- Helper function to show version in a nice format
showVersion :: ClapVersion -> String
showVersion ClapVersion{..} = show major ++ "." ++ show minor ++ "." ++ show revision

-- Helper function to call the init function pointer
callInitFunction :: ClapPluginEntry -> CString -> IO Bool
callInitFunction entry path = do
  let initFn = mkInitFnFromFunPtr (init entry)
  initFn path

-- Helper function to call the deinit function pointer
callDeinitFunction :: ClapPluginEntry -> IO ()
callDeinitFunction entry = do
  let deinitFn = mkDeinitFnFromFunPtr (deinit entry)
  deinitFn