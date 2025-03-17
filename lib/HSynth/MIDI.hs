{-# LANGUAGE OverloadedStrings #-}

module HSynth.MIDI (
  MIDIEvent (..),
  Note (..),
  Velocity,
  Channel,
  handleMIDIEvent,
  decodeMIDIMessage,
  midiBytesToEvent,
) where

import Control.Concurrent (ThreadId)
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Sound.MIDI.Message qualified as Midi
import Sound.MIDI.Message.Channel qualified as Channel
import Sound.MIDI.Message.Channel.Voice qualified as Voice

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

-- | Convert MIDI message to our MIDIEvent type
decodeMIDIMessage :: Midi.T -> Maybe MIDIEvent
decodeMIDIMessage msg = case msg of
  Midi.Channel channelMsg -> decodeChannelMessage channelMsg
  _ -> Nothing -- Ignore other types of messages like SysEx

-- | Decode a channel message
decodeChannelMessage :: Channel.T -> Maybe MIDIEvent
decodeChannelMessage msg =
  let chanNum = fromIntegral $ Channel.fromChannel (Channel.toChannel msg)
   in case Channel.messageBody msg of
        Channel.Voice voiceMsg -> decodeVoiceMessage chanNum voiceMsg
        _ -> Nothing -- Ignore mode messages

-- | Decode a voice message
decodeVoiceMessage :: Channel -> Voice.T -> Maybe MIDIEvent
decodeVoiceMessage chan msg = case msg of
  Voice.NoteOn note vel
    | vel == 0 -> Just $ NoteOff (Note $ fromIntegral note) chan
    | otherwise -> Just $ NoteOn (Note $ fromIntegral note) (fromIntegral vel) chan
  Voice.NoteOff note _ -> Just $ NoteOff (Note $ fromIntegral note) chan
  Voice.PitchBend val -> Just $ PitchBend chan (fromIntegral val - 8192) -- Center at 0
  Voice.Control controller value -> Just $ ControlChange chan (fromIntegral controller) (fromIntegral value)
  _ -> Nothing -- Ignore other voice messages

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
        let value = (fromIntegral msb `Data.Bits.shiftL` 7) .|. fromIntegral lsb - 8192
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

-- | Bit shift operators
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

{- | Start a MIDI listener that will call the callback for each MIDI event
This is a placeholder for now
-}
startMIDIListener :: IO (Maybe ThreadId)
startMIDIListener = do
  putStrLn "MIDI listener placeholder - not actually connecting to any devices"
  putStrLn "This would be implemented with a proper MIDI library"
  return Nothing