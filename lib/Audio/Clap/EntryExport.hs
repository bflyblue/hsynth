{-# LANGUAGE ForeignFunctionInterface #-}

module Audio.Clap.EntryExport (
  -- * Entry point creation
  makePluginEntry,

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
The CLAP version is automatically set to the current version from the headers.

This function is designed to be called ONCE during plugin initialization
to create the static plugin entry point that will be exported to CLAP hosts.
Typically used with unsafePerformIO and NOINLINE for creating the global
clap_entry symbol.

MEMORY MANAGEMENT: This function creates FunPtrs that are not automatically garbage collected.
According to the Haskell FFI specification, these would normally need to be freed with
freeHaskellFunctionPtr when no longer needed. However, since these function pointers are
part of the plugin's entry point structure and are expected to exist for the entire
lifetime of the plugin library, they don't need to be explicitly freed - they will be
cleaned up automatically when the host unloads the plugin library.

Usage example:

> {\-# NOINLINE clapEntry #-\}
> clapEntry :: Ptr ClapPluginEntry
> clapEntry = unsafePerformIO $ makePluginEntry myInit myDeinit myGetFactory
>
> foreign export ccall "clap_entry" clapEntry :: Ptr ClapPluginEntry
-}
makePluginEntry ::
  (CString -> IO Bool) ->
  IO () ->
  (CString -> IO (Ptr ())) ->
  IO (Ptr ClapPluginEntry)
makePluginEntry initFn deinitFn factoryFn = do
  -- Create function pointers from the Haskell functions
  initPtr <- mkInitFn initFn
  deinitPtr <- mkDeinitFn deinitFn
  factoryPtr <- mkGetFactoryFn factoryFn

  -- Allocate memory for the entry structure
  entryPtr <- malloc

  -- Populate the structure with function pointers and current CLAP version
  poke
    entryPtr
    ClapPluginEntry
      { clapVersion = clapCurrentVersion
      , init = initPtr
      , deinit = deinitPtr
      , getFactory = factoryPtr
      }

  return entryPtr

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