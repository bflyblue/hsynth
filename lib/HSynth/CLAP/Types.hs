{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HSynth.CLAP.Types where

import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import HSynth.Synth (Note (..), Velocity)

-- | Constants for CLAP event space IDs
clapCoreEventSpaceId :: Word16
clapCoreEventSpaceId = 0

-- | Constants for CLAP event types
clapEventNoteOn :: Word16
clapEventNoteOn = 0

clapEventNoteOff :: Word16
clapEventNoteOff = 1

clapEventNoteChoke :: Word16
clapEventNoteChoke = 2

clapEventNoteEnd :: Word16
clapEventNoteEnd = 3

-- | Event header structure
data ClapEventHeader = ClapEventHeader
  { eventSize :: Word32
  -- ^ Event size including this header
  , eventTime :: Word32
  -- ^ Sample offset within the buffer for this event
  , eventSpaceId :: Word16
  -- ^ Event space ID
  , eventType :: Word16
  -- ^ Event type
  , eventFlags :: Word32
  -- ^ Event flags
  }

instance Storable ClapEventHeader where
  sizeOf _ = 16 -- 4 + 4 + 2 + 2 + 4

  alignment _ = 4

  peek ptr = do
    size <- peekByteOff ptr 0
    time <- peekByteOff ptr 4
    spaceId <- peekByteOff ptr 8
    evType <- peekByteOff ptr 10
    flags <- peekByteOff ptr 12
    return $ ClapEventHeader size time spaceId evType flags

  poke ptr (ClapEventHeader size time spaceId evType flags) = do
    pokeByteOff ptr 0 size
    pokeByteOff ptr 4 time
    pokeByteOff ptr 8 spaceId
    pokeByteOff ptr 10 evType
    pokeByteOff ptr 12 flags

-- | Note event structure
data ClapEventNote = ClapEventNote
  { noteHeader :: ClapEventHeader
  , noteId :: Int32
  -- ^ Host provided note id >= 0, or -1 if unspecified/wildcard
  , portIndex :: Int16
  -- ^ Port index, -1 for wildcard
  , channel :: Int16
  -- ^ 0-15 (like MIDI), -1 for wildcard
  , key :: Int16
  -- ^ 0-127 (like MIDI), -1 for wildcard
  , velocity :: Double
  -- ^ 0-1 range (normalized, unlike MIDI's 0-127)
  }

instance Storable ClapEventNote where
  sizeOf _ = 32 -- 16 (header) + 4 + 2 + 2 + 2 + 8

  alignment _ = 8 -- Alignment for Double

  peek ptr = do
    header <- peek (castPtr ptr)
    nId <- peekByteOff ptr 16
    pIdx <- peekByteOff ptr 20
    ch <- peekByteOff ptr 22
    k <- peekByteOff ptr 24
    vel <- peekByteOff ptr 26
    return $ ClapEventNote header nId pIdx ch k vel

  poke ptr (ClapEventNote header nId pIdx ch k vel) = do
    poke (castPtr ptr) header
    pokeByteOff ptr 16 nId
    pokeByteOff ptr 20 pIdx
    pokeByteOff ptr 22 ch
    pokeByteOff ptr 24 k
    pokeByteOff ptr 26 vel

-- | Convenience functions to create CLAP note events
mkNoteOnEvent :: Int16 -> Int16 -> Double -> Word32 -> ClapEventNote
mkNoteOnEvent key' channel' velocity' time' =
  ClapEventNote
    { noteHeader =
        ClapEventHeader
          { eventSize = 32
          , eventTime = time'
          , eventSpaceId = clapCoreEventSpaceId
          , eventType = clapEventNoteOn
          , eventFlags = 0
          }
    , noteId = -1 -- Let the host assign an ID
    , portIndex = 0
    , channel = channel'
    , key = key'
    , velocity = velocity'
    }

mkNoteOffEvent :: Int16 -> Int16 -> Word32 -> ClapEventNote
mkNoteOffEvent key' channel' time' =
  ClapEventNote
    { noteHeader =
        ClapEventHeader
          { eventSize = 32
          , eventTime = time'
          , eventSpaceId = clapCoreEventSpaceId
          , eventType = clapEventNoteOff
          , eventFlags = 0
          }
    , noteId = -1 -- Will match by key and channel
    , portIndex = 0
    , channel = channel'
    , key = key'
    , velocity = 0 -- Ignored for note-off
    }

-- | Helper to convert internal note to CLAP note
noteToKey :: Note -> Int16
noteToKey note = fromIntegral $ noteNumber note

-- | Helper to convert velocity
velocityToCLAP :: Velocity -> Double
velocityToCLAP vel = fromIntegral vel / 127.0