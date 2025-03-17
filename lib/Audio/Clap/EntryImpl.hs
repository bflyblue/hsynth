module Audio.Clap.EntryImpl where

import Audio.Clap.Entry
import Audio.Clap.Version
import Control.Concurrent.MVar
import Control.Monad (when)
import Foreign
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (init)

-- Global state for our CLAP entry point implementation
data EntryState = EntryState
  { refCount :: !Int
  , initFunPtr :: Maybe (FunPtr ClapInitFn)
  , deinitFunPtr :: Maybe (FunPtr ClapDeinitFn)
  , getFactoryFunPtr :: Maybe (FunPtr ClapGetFactoryFn)
  }

-- Create an initial empty state
emptyState :: EntryState
emptyState =
  EntryState
    { refCount = 0
    , initFunPtr = Nothing
    , deinitFunPtr = Nothing
    , getFactoryFunPtr = Nothing
    }

-- Global state for our plugin, protected by an MVar for thread safety
{-# NOINLINE globalState #-}
globalState :: MVar EntryState
globalState = unsafePerformIO $ newMVar emptyState

-- Our implementation of the plugin's init function
pluginInit :: CString -> IO Bool
pluginInit pluginPath = do
  -- Acquire mutex and update state
  modifyMVar globalState $ \state -> do
    let newCount = refCount state + 1

    -- Create function pointers only on the first init call
    (newState, shouldInit) <-
      if newCount == 1
        then do
          -- This is the first call, create function pointers and do initialization
          putStrLn "First init call, creating function pointers..."
          return (state{refCount = newCount}, True)
        else do
          -- Subsequent calls just increment the counter
          putStrLn $ "Init called again, ref count: " ++ show newCount
          return (state{refCount = newCount}, False)

    -- Perform any initialization if needed (first time only)
    when shouldInit $ do
      path <- peekCString pluginPath
      putStrLn $ "Initializing plugin from path: " ++ path
    -- Actual initialization logic would go here

    -- Return the updated state and success
    return (newState, True)

-- Our implementation of the plugin's deinit function
pluginDeinit :: IO ()
pluginDeinit = do
  -- Acquire mutex and update state
  modifyMVar_ globalState $ \state -> do
    let newCount = refCount state - 1

    -- Clean up resources on the last deinit call
    when (newCount == 0) $ do
      putStrLn "Last deinit call, freeing function pointers..."
      -- Free any function pointers we created
      maybe (return ()) freeHaskellFunPtr (initFunPtr state)
      maybe (return ()) freeHaskellFunPtr (deinitFunPtr state)
      maybe (return ()) freeHaskellFunPtr (getFactoryFunPtr state)
    -- Additional cleanup could go here

    if newCount >= 0
      then do
        putStrLn $ "Deinit called, ref count: " ++ show newCount
        return
          state
            { refCount = newCount
            , initFunPtr = if newCount == 0 then Nothing else initFunPtr state
            , deinitFunPtr = if newCount == 0 then Nothing else deinitFunPtr state
            , getFactoryFunPtr = if newCount == 0 then Nothing else getFactoryFunPtr state
            }
      else do
        putStrLn "Warning: deinit called more times than init!"
        return state

-- Our implementation of the plugin's getFactory function
pluginGetFactory :: CString -> IO (Ptr ())
pluginGetFactory factoryIdPtr = do
  factoryId <- peekCString factoryIdPtr
  putStrLn $ "GetFactory called for: " ++ factoryId

  -- Here we would return the appropriate factory based on the ID
  -- For now just return null
  return nullPtr

-- Initialize the entry point structure with our implementations
initializeEntryPoint :: IO (Ptr ClapPluginEntry)
initializeEntryPoint = do
  -- Create new function pointers
  initPtr <- mkInitFn pluginInit
  deinitPtr <- mkDeinitFn pluginDeinit
  factoryPtr <- mkGetFactoryFn pluginGetFactory

  -- Store them in our global state for later cleanup
  modifyMVar_ globalState $ \state ->
    return
      state
        { initFunPtr = Just initPtr
        , deinitFunPtr = Just deinitPtr
        , getFactoryFunPtr = Just factoryPtr
        }

  -- Create and populate the entry structure
  entryPtr <- malloc
  poke
    entryPtr
    ClapPluginEntry
      { clapVersion = clapCurrentVersion
      , init = initPtr
      , deinit = deinitPtr
      , getFactory = factoryPtr
      }

  return entryPtr