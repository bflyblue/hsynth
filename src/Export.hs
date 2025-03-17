{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Audio.Clap.Entry
import Audio.Clap.Factory
import Audio.Clap.Version
import Control.Concurrent.MVar
import Control.Monad
import Foreign
import Foreign.C
import System.IO
import System.IO.Unsafe
import Prelude hiding (init)

-- Global state for our plugin
data PluginState = PluginState
  { refCount :: !Int
  , isInitialized :: !Bool
  }

-- Create an initial empty state
emptyState :: PluginState
emptyState =
  PluginState
    { refCount = 0
    , isInitialized = False
    }

-- Global state for our plugin, protected by an MVar for thread safety
{-# NOINLINE globalState #-}
globalState :: MVar PluginState
globalState = unsafePerformIO $ newMVar emptyState

-- Our implementation of the plugin's init function
pluginInit :: CString -> IO Bool
pluginInit pluginPath = do
  path <- peekCString pluginPath

  -- Acquire mutex and update state
  modifyMVar globalState $ \state -> do
    let newCount = refCount state + 1

    -- Only do actual initialization on first call
    when (newCount == 1) $ do
      hPutStrLn stderr $ "Initializing HSynth plugin from: " ++ path
    -- Here you would do your actual plugin initialization

    hPutStrLn stderr $ "Init called, ref count now: " ++ show newCount

    -- Return updated state and success
    return (state{refCount = newCount, isInitialized = True}, True)

-- Our implementation of the plugin's deinit function
pluginDeinit :: IO ()
pluginDeinit = do
  -- Acquire mutex and update state
  modifyMVar_ globalState $ \state -> do
    let newCount = refCount state - 1

    -- Only do actual cleanup on last call
    when (newCount == 0) $ do
      hPutStrLn stderr "Cleaning up HSynth plugin resources"
    -- Here you would do your actual plugin cleanup

    hPutStrLn stderr $ "Deinit called, ref count now: " ++ show newCount

    -- Return updated state
    return state{refCount = newCount, isInitialized = newCount > 0}

-- | Plugin factory implementation

-- Function to return the number of plugins we provide
pluginCountFn :: Ptr ClapPluginFactory -> IO CUInt
pluginCountFn _ = return 1 -- We only have one plugin

-- Function to return the plugin descriptor
pluginDescriptorFn :: Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)
pluginDescriptorFn _ index
  | index == 0 = return pluginDescriptor
  | otherwise = return nullPtr

-- | Our plugin ID as a String, used for comparison in createPluginFn
pluginIdStr :: String
pluginIdStr = "org.haskell.hsynth"

-- Function to create a plugin instance (dummy implementation for now)
-- In a real implementation, we would check if requestedId matches our plugin ID
-- and create a plugin instance if it does
createPluginFn :: Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ())
createPluginFn _ _ requestedId = do
  reqIdStr <- peekCString requestedId
  if reqIdStr == pluginIdStr
    then do
      hPutStrLn stderr "Plugin ID matched, but we don't implement actual plugin instances yet"
      -- For a real plugin, we would allocate and return a plugin instance here
      return nullPtr
    else do
      hPutStrLn stderr $ "Plugin ID didn't match (requested: " ++ reqIdStr ++ ", ours: " ++ pluginIdStr ++ ")"
      return nullPtr

-- Plugin descriptor with information about our plugin
{-# NOINLINE pluginDescriptor #-}
pluginDescriptor :: Ptr ClapPluginDescriptor
pluginDescriptor = unsafePerformIO $ do
  -- Create the required strings
  pluginId <- newCString pluginIdStr
  pluginName <- newCString "HSynth"
  vendor <- newCString "Haskell Audio"
  url <- newCString "https://github.com/yourusername/hsynth"
  manualUrl <- newCString "https://github.com/yourusername/hsynth"
  supportUrl <- newCString "https://github.com/yourusername/hsynth"
  version <- newCString "0.1.0"
  description <- newCString "A minimal synthesizer written in Haskell"

  -- Define plugin features using the constants from the CLAP API
  synthFeature <- newCString clapPluginFeatureSynthesizer
  instrumentFeature <- newCString clapPluginFeatureInstrument
  stereoFeature <- newCString clapPluginFeatureStereo

  makePluginDescriptor
    Audio.Clap.Version.clapCurrentVersion
    pluginId
    pluginName
    vendor
    url
    manualUrl
    supportUrl
    version
    description
    [synthFeature, instrumentFeature, stereoFeature]

-- Our plugin factory that hosts can query to get plugin info
{-# NOINLINE pluginFactory #-}
pluginFactory :: Ptr ClapPluginFactory
pluginFactory =
  unsafePerformIO $
    makePluginFactory pluginCountFn pluginDescriptorFn createPluginFn

-- | The getFactory implementation that returns our factory when requested
pluginGetFactory :: CString -> IO (Ptr ())
pluginGetFactory factoryIdPtr = do
  factoryId <- peekCString factoryIdPtr
  hPutStrLn stderr $ "Factory requested: " ++ factoryId

  -- Get the CLAP plugin factory ID
  if factoryId == clapPluginFactoryIdStr
    then do
      hPutStrLn stderr "Returning plugin factory"
      return $ castPtr pluginFactory
    else do
      hPutStrLn stderr $ "Unknown factory ID: " ++ factoryId
      return nullPtr

{- | The actual entry point structure that will be exported to C.
This follows the Haskell FFI specification for static exports.
The entry point is created once during module initialization and
persists for the lifetime of the loaded library.

The NOINLINE pragma ensures that the entry structure isn't duplicated
by the optimizer, maintaining a single consistent address for the symbol.
-}
{-# NOINLINE clapEntry #-}
clapEntry :: Ptr ClapPluginEntry
clapEntry = unsafePerformIO $ makePluginEntry pluginInit pluginDeinit pluginGetFactory

-- Export the entry point for CLAP hosts to find
foreign export ccall "clap_entry" clapEntry :: Ptr ClapPluginEntry