module HSynth.CLAP.Plugin (
  initCLAPPlugin,
  testCLAPIntegration,
) where

import HSynth.CLAP.Bindings
import HSynth.Synth (Synth)
import HSynth.Synth qualified as Synth

-- | Initialize our CLAP plugin with our synthesizer
initCLAPPlugin :: Synth -> IO Bool
initCLAPPlugin synth = do
  -- For now, just a stub that will eventually create a real CLAP plugin
  -- Later we'll use our synth instance and bind it to CLAP callbacks
  testCLAPPlugin

-- | Simple test to verify we can interact with CLAP headers
testCLAPIntegration :: IO ()
testCLAPIntegration = do
  putStrLn "Testing CLAP integration..."

  -- Test if we can access CLAP version from headers
  version <- getCLAPVersion
  putStrLn version

  -- Test if we can create a plugin descriptor
  success <- testCLAPPlugin
  if success
    then putStrLn "CLAP integration test successful!"
    else putStrLn "CLAP integration test failed."

  putStrLn "Test complete."