{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module HSynth.CLAP.Bindings where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable

import HSynth.CLAP.Types
import HSynth.Synth (Note (..), Velocity, createSynth, noteOff, noteOn)
import HSynth.Synth qualified as Synth

-- | Plugin descriptor type
data ClapPluginDescriptor = ClapPluginDescriptor
  { pluginId :: String
  , pluginName :: String
  , pluginVendor :: String
  , pluginUrl :: String
  , pluginManualUrl :: String
  , pluginSupportUrl :: String
  , pluginVersion :: String
  , pluginDescription :: String
  }

-- | Create a CLAP plugin descriptor
createPluginDescriptor :: IO ClapPluginDescriptor
createPluginDescriptor =
  return $
    ClapPluginDescriptor
      { pluginId = "com.shaun.hsynth"
      , pluginName = "HSynth"
      , pluginVendor = "Shaun Sharples"
      , pluginUrl = "https://github.com/shaun-sharples/hsynth"
      , pluginManualUrl = "https://github.com/shaun-sharples/hsynth/docs"
      , pluginSupportUrl = "https://github.com/shaun-sharples/hsynth/issues"
      , pluginVersion = "0.1.0"
      , pluginDescription = "A Haskell-based synthesizer"
      }

{- | Process a CLAP note event
Instead of using pointers for Synth, we'll take and return it directly
-}
processCLAPNoteEvent :: Ptr ClapEventNote -> Synth.Synth -> IO Synth.Synth
processCLAPNoteEvent eventPtr synth = do
  noteEvent <- peek eventPtr

  let newSynth = case eventType (noteHeader noteEvent) of
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

  -- Return the updated synth state
  return newSynth

-- | Create a CLAP plugin with our synthesizer
initializeCLAPPlugin :: Double -> IO Bool
initializeCLAPPlugin sampleRate = do
  -- Create our synth with the provided sample rate
  let synth = createSynth sampleRate

  -- Set up plugin state
  -- In a real implementation, we would:
  -- 1. Create plugin descriptor in C format
  -- 2. Set up function pointers for the CLAP interface
  -- 3. Register audio and MIDI ports
  -- 4. Store the synth instance in the plugin state

  -- For now this is still a placeholder
  putStrLn $ "Initializing CLAP plugin with sample rate: " ++ show sampleRate
  return True

-- | Get the CLAP version (placeholder)
getCLAPVersion :: IO String
getCLAPVersion = do
  return "CLAP 1.1.0"

-- | Basic test that will succeed for now
testCLAPPlugin :: IO Bool
testCLAPPlugin = do
  putStrLn "Testing CLAP plugin creation"
  initializeCLAPPlugin 44100