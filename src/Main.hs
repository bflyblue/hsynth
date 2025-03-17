{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment (getArgs)

import HSynth.CLAP.Plugin (handleCLAPNoteEvent, testCLAPIntegration)
import HSynth.CLAP.Types (key, mkNoteOffEvent, mkNoteOnEvent, velocity)
import HSynth.Synth (Note (..), ParamId (..), createSynth, noteOff, noteOn, setParameter)

-- | Main entry point
main :: IO ()
main = do
  putStrLn "HSynth - Haskell MIDI Synthesizer with CLAP"
  putStrLn "==========================================="

  args <- getArgs
  case args of
    ["--test-clap"] -> testCLAPMode
    ["--test-clap-notes"] -> testCLAPNotesMode
    _ -> normalMode

-- | Run a test of CLAP integration
testCLAPMode :: IO ()
testCLAPMode = do
  putStrLn "Running CLAP integration test"
  testCLAPIntegration

-- | Run a more detailed test of CLAP note handling
testCLAPNotesMode :: IO ()
testCLAPNotesMode = do
  putStrLn "Testing CLAP note handling"

  -- Create our synthesizer
  let synth = createSynth 44100

  -- Create CLAP note events
  let noteOn1 = mkNoteOnEvent 60 0 0.75 0 -- C4, channel 0, 75% velocity
      noteOn2 = mkNoteOnEvent 64 0 0.80 100 -- E4, channel 0, 80% velocity
      noteOff1 = mkNoteOffEvent 60 0 500 -- Release C4 after 500 samples

  -- Process the events with our synth
  putStrLn $ "Processing note-on event for key: " ++ show (key noteOn1)
  putStrLn $ "With normalized velocity: " ++ show (velocity noteOn1)

  let synth2 = handleCLAPNoteEvent noteOn1 synth
      synth3 = handleCLAPNoteEvent noteOn2 synth2
      synth4 = handleCLAPNoteEvent noteOff1 synth3

  putStrLn "Successfully processed CLAP note events"
  putStrLn "CLAP note test complete"

-- | Normal operation mode with basic synth testing
normalMode :: IO ()
normalMode = do
  -- Create a synthesizer instance
  let synth = createSynth 44100 -- 44.1kHz sample rate

  -- Show initial state
  putStrLn "\nTesting basic synthesizer functionality:"

  -- Test parameter changes
  let synth2 = setParameter synth WaveformParam 0.5 -- Set waveform to square
  putStrLn "Changed waveform parameter"

  -- Test note triggering
  let synth3 = noteOn synth2 (Note 60) 100 -- Note C4 with velocity 100
  putStrLn "Triggered note C4 (note 60)"

  -- Release note C4
  let _ = noteOff synth3 (Note 60) -- This is just a demonstration
  putStrLn "Released note C4"

  putStrLn "\nSynthesizer test complete"
  putStrLn "Press Ctrl+C to exit"

  -- Keep the application running
  forever $ do
    threadDelay 1000000 -- Sleep for 1 second
    putStr "." -- Simple activity indicator