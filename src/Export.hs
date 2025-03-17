{-# LANGUAGE ForeignFunctionInterface #-}

module Export where

import Audio.Clap.Entry hiding (clapEntry)
import Audio.Clap.EntryExport
import Audio.Clap.Version
import Control.Concurrent.MVar
import Control.Monad
import Foreign
import Foreign.C.String
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

-- Our implementation of the plugin's getFactory function
pluginGetFactory :: CString -> IO (Ptr ())
pluginGetFactory factoryIdPtr = do
  factoryId <- peekCString factoryIdPtr
  hPutStrLn stderr $ "Factory requested: " ++ factoryId

  -- Here you would return the appropriate factory type
  -- For now we just return null
  return nullPtr

-- Create the function pointers for our plugin
-- These are created at load time and kept throughout the plugin's lifecycle
{-# NOINLINE pluginFunctionPtrs #-}
pluginFunctionPtrs :: (FunPtr (CString -> IO Bool), FunPtr (IO ()), FunPtr (CString -> IO (Ptr ())))
pluginFunctionPtrs = unsafePerformIO $ do
  initPtr <- mkInitFunPtr pluginInit
  deinitPtr <- mkDeinitFunPtr pluginDeinit
  factoryPtr <- mkGetFactoryFunPtr pluginGetFactory
  return (initPtr, deinitPtr, factoryPtr)

-- The actual entry point structure that will be exported to C
{-# NOINLINE clapEntry #-}
clapEntry :: Ptr ClapPluginEntry
clapEntry = unsafePerformIO $ do
  let (initPtr, deinitPtr, factoryPtr) = pluginFunctionPtrs
  makePluginEntry clapCurrentVersion initPtr deinitPtr factoryPtr

-- Export the entry point for CLAP hosts to find
foreign export ccall "clap_entry" clapEntry :: Ptr ClapPluginEntry