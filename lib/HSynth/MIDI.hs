{-# LANGUAGE OverloadedStrings #-}

module HSynth.MIDI (
  MIDIEvent (..),
  Note (..),
  Velocity,
  Channel,
  handleMIDIEvent,
  midiBytesToEvent,
) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)

-- | Basic MIDI events we care about
data MIDIEvent
  = NoteOn Note Velocity Channel
  | NoteOff Note Channel
  | -- | Value ranges from -8192 to 8191
    PitchBend Channel Int
  | ControlChange Channel Word8 Word8
  deriving (Show, Eq)

-- | MIDI note representation
newtype Note = Note {noteNumber :: Word8}
  deriving (Show, Eq, Ord)

type Velocity = Word8
type Channel = Word8

-- | Convert raw MIDI bytes to our MIDIEvent type
midiBytesToEvent :: [Word8] -> Maybe MIDIEvent
midiBytesToEvent bytes = case bytes of
  [status, note, vel]
    | status >= 0x80 && status <= 0x8F -> -- Note Off
        Just $ NoteOff (Note note) (status - 0x80)
    | status >= 0x90 && status <= 0x9F -> -- Note On
        if vel == 0
          then Just $ NoteOff (Note note) (status - 0x90)
          else Just $ NoteOn (Note note) vel (status - 0x90)
  [status, controller, value]
    | status >= 0xB0 && status <= 0xBF -> -- Control Change
        Just $ ControlChange (status - 0xB0) controller value
  [status, lsb, msb]
    | status >= 0xE0 && status <= 0xEF -> -- Pitch Bend
        let msb' = fromIntegral msb :: Int
            lsb' = fromIntegral lsb :: Int
            value = (msb' `shiftL` 7) .|. lsb' - 8192
         in Just $ PitchBend (status - 0xE0) value
  _ -> Nothing

-- | Process a MIDI event
handleMIDIEvent :: MIDIEvent -> IO ()
handleMIDIEvent event = case event of
  NoteOn note vel channel ->
    putStrLn $ "Note On: " ++ show (noteNumber note) ++ " with velocity " ++ show vel ++ " on channel " ++ show channel
  NoteOff note channel ->
    putStrLn $ "Note Off: " ++ show (noteNumber note) ++ " on channel " ++ show channel
  PitchBend channel value ->
    putStrLn $ "Pitch Bend: " ++ show value ++ " on channel " ++ show channel
  ControlChange channel controller value ->
    putStrLn $ "Control Change: controller " ++ show controller ++ " value " ++ show value ++ " on channel " ++ show channel