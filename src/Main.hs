{-# LANGUAGE RecordWildCards #-}

module Main where

import Audio.Clap.Entry

main :: IO ()
main = do
  putStrLn "HSynth CLAP Plugin Test"
  putStrLn "-----------------------"

  putStrLn $ "CLAP Version: " ++ showVersion clapCurrentVersion
  putStrLn $ "Major: " ++ show clapVersionMajor
  putStrLn $ "Minor: " ++ show clapVersionMinor
  putStrLn $ "Revision: " ++ show clapVersionRevision
  putStrLn $ "Compatible: " ++ show (clapVersionIsCompatible clapCurrentVersion)

  putStrLn "Done."

-- Helper function to show version in a nice format
showVersion :: ClapVersion -> String
showVersion ClapVersion{..} = show major ++ "." ++ show minor ++ "." ++ show revision