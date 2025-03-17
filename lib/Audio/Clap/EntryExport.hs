{-# LANGUAGE ForeignFunctionInterface #-}

module Audio.Clap.EntryExport (
  -- * Entry point creation
  makePluginEntry,

  -- * Helper functions
  mkInitFunPtr,
  mkDeinitFunPtr,
  mkGetFactoryFunPtr,

  -- * Dynamic function imports (for testing)
  mkInitFnFromFunPtr,
  mkDeinitFnFromFunPtr,
  mkGetFactoryFnFromFunPtr,
) where

import Audio.Clap.Entry
import Audio.Clap.Version
import Foreign
import Foreign.C.String
import Prelude hiding (init)

{- | Create a fully initialized CLAP plugin entry point structure.
This allocates the structure and populates it with the given function pointers.
-}
makePluginEntry ::
  ClapVersion ->
  FunPtr (CString -> IO Bool) ->
  FunPtr (IO ()) ->
  FunPtr (CString -> IO (Ptr ())) ->
  IO (Ptr ClapPluginEntry)
makePluginEntry version initFn deinitFn factoryFn = do
  -- Allocate memory for the entry structure
  entryPtr <- malloc

  -- Populate the structure with function pointers
  poke
    entryPtr
    ClapPluginEntry
      { clapVersion = version
      , init = initFn
      , deinit = deinitFn
      , getFactory = factoryFn
      }

  return entryPtr

-- | Create a function pointer for an init function
mkInitFunPtr :: (CString -> IO Bool) -> IO (FunPtr (CString -> IO Bool))
mkInitFunPtr = mkInitFn

-- | Create a function pointer for a deinit function
mkDeinitFunPtr :: IO () -> IO (FunPtr (IO ()))
mkDeinitFunPtr = mkDeinitFn

-- | Create a function pointer for a getFactory function
mkGetFactoryFunPtr :: (CString -> IO (Ptr ())) -> IO (FunPtr (CString -> IO (Ptr ())))
mkGetFactoryFunPtr = mkGetFactoryFn

{- | Convert a function pointer back to a callable init function
Useful for testing or when you need to call the function directly
-}
foreign import ccall "dynamic"
  mkInitFnFromFunPtr :: FunPtr (CString -> IO Bool) -> (CString -> IO Bool)

-- | Convert a function pointer back to a callable deinit function
foreign import ccall "dynamic"
  mkDeinitFnFromFunPtr :: FunPtr (IO ()) -> IO ()

-- | Convert a function pointer back to a callable getFactory function
foreign import ccall "dynamic"
  mkGetFactoryFnFromFunPtr :: FunPtr (CString -> IO (Ptr ())) -> (CString -> IO (Ptr ()))