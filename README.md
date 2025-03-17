# HSynth - Haskell MIDI Synthesizer

A Haskell-based analog synthesizer that can communicate with DAWs using MIDI.

## Features

- MIDI input support - receive notes and control changes from your DAW
- Basic synthesizer with different waveforms (sine, square, saw, triangle)
- Simple ADSR envelope 
- Placeholder for VST plugin implementation

## Requirements

- GHC (Glasgow Haskell Compiler) 9.4 or later
- Cabal 3.6 or later
- A compatible MIDI device or virtual MIDI port

## Building

This project uses Cabal for builds:

```bash
cabal build
```

## Running

To start the MIDI listener:

```bash
cabal run
```

The application will list available MIDI devices and start listening for MIDI events.

## Project Structure

- `lib/HSynth/MIDI.hs` - MIDI message handling
- `lib/HSynth/Synth.hs` - Synthesizer core with audio generation
- `lib/HSynth/VST.hs` - VST plugin interface (placeholder)
- `src/Main.hs` - Main application entry point

## Future Development

- Implement full VST plugin support using FFI
- Add audio output through sound card
- Implement more synthesizer features (filters, effects, etc.)
- Add GUI for standalone operation

## Notes on VST Plugin Development

Creating a full VST plugin in Haskell requires:

1. C FFI bindings to the VST SDK
2. Implementation of VST entry points and callbacks
3. Export as a shared library (.dll, .so, or .vst)

This is challenging in Haskell and may require additional libraries or C++ wrapper code.

## License

[Your license here] 