module HSynth.Synth (
  Synth,
  createSynth,
  noteOn,
  noteOff,
  setSynthParameter,
  renderAudio,
) where

import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Word (Word8)
import HSynth.MIDI (Channel, Note (..), Velocity)

-- | Basic oscillator waveforms
data Waveform = Sine | Square | Saw | Triangle
  deriving (Show, Eq, Enum)

-- | Voice represents a single note being played
data Voice = Voice
  { voiceNote :: Note
  , voiceVelocity :: Velocity
  , voiceActive :: Bool
  , voicePhase :: Double
  -- ^ Current phase of the oscillator (0 to 1)
  }

-- | Synth state containing all voices and parameters
data Synth = Synth
  { synthVoices :: [Voice]
  , synthWaveform :: Waveform
  , synthAttack :: Double
  -- ^ Attack time in seconds
  , synthDecay :: Double
  -- ^ Decay time in seconds
  , synthSustain :: Double
  -- ^ Sustain level (0 to 1)
  , synthRelease :: Double
  -- ^ Release time in seconds
  , synthCutoff :: Double
  -- ^ Filter cutoff frequency
  , synthResonance :: Double
  -- ^ Filter resonance
  , synthSampleRate :: Double
  -- ^ Sample rate in Hz
  }

-- | Create a new synth with default parameters
createSynth :: Double -> Synth
createSynth sampleRate =
  Synth
    { synthVoices = []
    , synthWaveform = Sine
    , synthAttack = 0.01
    , synthDecay = 0.1
    , synthSustain = 0.7
    , synthRelease = 0.3
    , synthCutoff = 12000
    , synthResonance = 0.5
    , synthSampleRate = sampleRate
    }

-- | Handle a note on event
noteOn :: Synth -> Note -> Velocity -> Channel -> Synth
noteOn synth note vel _channel =
  let voice =
        Voice
          { voiceNote = note
          , voiceVelocity = vel
          , voiceActive = True
          , voicePhase = 0
          }
   in synth{synthVoices = voice : synthVoices synth}

-- | Handle a note off event
noteOff :: Synth -> Note -> Channel -> Synth
noteOff synth note _channel =
  let updateVoice v =
        if voiceNote v == note
          then v{voiceActive = False}
          else v
   in synth{synthVoices = map updateVoice (synthVoices synth)}

-- | Update a synth parameter
setSynthParameter :: Synth -> Word8 -> Word8 -> Synth
setSynthParameter synth cc value =
  case cc of
    1 -> synth{synthWaveform = toEnum (min 3 (fromIntegral value `div` 32))}
    74 -> synth{synthCutoff = 20 + (fromIntegral value / 127) * 19980}
    71 -> synth{synthResonance = fromIntegral value / 127}
    _ -> synth -- Ignore unknown control changes

-- | Convert a MIDI note to frequency in Hz
noteToFreq :: Note -> Double
noteToFreq (Note n) = 440 * (2 ** ((fromIntegral n - 69) / 12))

-- | Calculate sample for a specific waveform at the given phase
sampleWaveform :: Waveform -> Double -> Double
sampleWaveform Sine phase = sin (phase * 2 * pi)
sampleWaveform Square phase
  | phase < 0.5 = 1
  | otherwise = -1
sampleWaveform Saw phase = 2 * phase - 1
sampleWaveform Triangle phase
  | phase < 0.25 = 4 * phase
  | phase < 0.75 = 2 - 4 * phase
  | otherwise = 4 * phase - 4

-- | Render audio samples for the synth
renderAudio :: Synth -> Int -> Vector Float
renderAudio synth numSamples = V.generate numSamples $ \i ->
  let active = filter voiceActive (synthVoices synth)
      sample =
        if null active
          then 0
          else
            sum [sampleVoice synth v (fromIntegral i) | v <- active] / fromIntegral (length active)
   in realToFrac sample

-- | Generate a sample for a voice
sampleVoice :: Synth -> Voice -> Double -> Double
sampleVoice synth voice sampleNum =
  let freq = noteToFreq (voiceNote voice)
      velocity = fromIntegral (voiceVelocity voice) / 127

      -- Calculate the phase increment for this frequency and sample rate
      phaseInc = freq / synthSampleRate synth

      -- Update phase and generate sample
      phase = voicePhase voice + phaseInc * sampleNum
      phase' = phase - fromIntegral (floor phase) -- wrap around to [0,1)
   in velocity * sampleWaveform (synthWaveform synth) phase'