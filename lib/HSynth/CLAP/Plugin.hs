module HSynth.CLAP.Plugin (
  initCLAPPlugin,
  testCLAPIntegration,
  handleCLAPNoteEvent,
  createCLAPPlugin,
  processAudioWithCLAP,
) where

import HSynth.CLAP.Bindings
import HSynth.CLAP.Types
import HSynth.Synth (Note (..), ParamId (..), Synth, Velocity, noteOff, noteOn, renderAudio, setParameter)
import HSynth.Synth qualified as Synth

import Data.Vector.Storable (Vector)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)

-- | Create a CLAP Plugin with our synthesizer
createCLAPPlugin :: Double -> IO Bool
createCLAPPlugin sampleRate = do
  -- Use our bindings module to initialize the plugin
  initializeCLAPPlugin sampleRate

-- | Initialize our CLAP plugin with an existing synthesizer instance
initCLAPPlugin :: Synth -> IO Bool
initCLAPPlugin synth = do
  -- For now, just a stub that will eventually create a real CLAP plugin
  -- Later we'll use our synth instance and bind it to CLAP callbacks
  putStrLn "Initializing CLAP plugin with existing synthesizer instance"
  return True

{- | Process an audio buffer using our synth through CLAP
This will eventually be called by the CLAP host
-}
processAudioWithCLAP :: Synth -> Int -> IO (Vector Float)
processAudioWithCLAP synth numSamples = do
  -- For now, just use our synth's renderAudio function directly
  -- In a real CLAP plugin, this would be called by the host's process callback
  return $ renderAudio synth numSamples

-- | Handle a CLAP note event and update our synthesizer accordingly
handleCLAPNoteEvent :: ClapEventNote -> Synth -> Synth
handleCLAPNoteEvent noteEvent synth =
  case eventType (noteHeader noteEvent) of
    -- Note-on event
    t
      | t == clapEventNoteOn ->
          let noteVal = fromIntegral (key noteEvent)
              -- Convert 0-1 velocity to 0-127 for our synth
              vel = round (velocity noteEvent * 127)
           in noteOn synth (Note noteVal) vel
    -- Note-off event
    t
      | t == clapEventNoteOff ->
          let noteVal = fromIntegral (key noteEvent)
           in noteOff synth (Note noteVal)
    -- Note-choke event (immediate silence)
    t
      | t == clapEventNoteChoke ->
          let noteVal = fromIntegral (key noteEvent)
           in noteOff synth (Note noteVal) -- For now, treat like note-off

    -- Other events
    _ -> synth

-- | Simple test to verify we can interact with CLAP headers
testCLAPIntegration :: IO ()
testCLAPIntegration = do
  putStrLn "Testing CLAP integration..."

  -- Test if we can access CLAP version from headers
  version <- getCLAPVersion
  putStrLn $ "CLAP Version: " ++ version

  -- Create a test note event
  let noteEvent = mkNoteOnEvent 60 0 0.8 0 -- Note C4, channel 0, velocity 0.8, time 0
      noteOffEvent = mkNoteOffEvent 60 0 100 -- Note C4 off at time 100
  putStrLn $ "Created test note-on event for key: " ++ show (key noteEvent)
  putStrLn $ "With velocity: " ++ show (velocity noteEvent)

  -- Test if we can create a plugin descriptor
  descriptor <- createPluginDescriptor
  putStrLn $ "Created plugin descriptor for: " ++ pluginName descriptor

  -- Test if we can create a plugin
  success <- createCLAPPlugin 44100
  if success
    then putStrLn "CLAP plugin creation successful!"
    else putStrLn "CLAP plugin creation failed."

  putStrLn "CLAP integration test complete."