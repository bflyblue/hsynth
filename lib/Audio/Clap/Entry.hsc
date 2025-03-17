{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Audio.Clap.Entry (
  -- * Entry point structure
  ClapPluginEntry(..),
  ClapInitFn,
  ClapDeinitFn,
  ClapGetFactoryFn,

  -- * Function pointer wrappers
  mkInitFn,
  mkDeinitFn,
  mkGetFactoryFn,

  -- * Entry point creation
  makePluginEntry,

  -- * Dynamic function imports (for testing)
  mkInitFnFromFunPtr,
  mkDeinitFnFromFunPtr,
  mkGetFactoryFnFromFunPtr,
) where

import Audio.Clap.Version
import Foreign
import Foreign.C.String
import Prelude hiding (init)

#include <clap/entry.h>

-- | The CLAP plugin entry point
data ClapPluginEntry = ClapPluginEntry
  { clapVersion :: !ClapVersion
  , init :: !(FunPtr (CString -> IO Bool))
  , deinit :: !(FunPtr (IO ()))
  , getFactory :: !(FunPtr (CString -> IO (Ptr ())))
  } deriving (Show)

instance Storable ClapPluginEntry where
  sizeOf _ = #{size clap_plugin_entry_t}
  alignment _ = #{alignment clap_plugin_entry_t}
  peek ptr = do
    clapVersion <- #{peek clap_plugin_entry_t, clap_version} ptr
    init <- #{peek clap_plugin_entry_t, init} ptr
    deinit <- #{peek clap_plugin_entry_t, deinit} ptr
    getFactory <- #{peek clap_plugin_entry_t, get_factory} ptr
    return ClapPluginEntry{..}
  poke ptr ClapPluginEntry{..} = do
    #{poke clap_plugin_entry_t, clap_version} ptr clapVersion
    #{poke clap_plugin_entry_t, init} ptr init
    #{poke clap_plugin_entry_t, deinit} ptr deinit
    #{poke clap_plugin_entry_t, get_factory} ptr getFactory

-- Function pointer wrappers
type ClapInitFn = CString -> IO Bool
type ClapDeinitFn = IO ()
type ClapGetFactoryFn = CString -> IO (Ptr ())

foreign import ccall "wrapper"
  mkInitFn :: ClapInitFn -> IO (FunPtr ClapInitFn)

foreign import ccall "wrapper"
  mkDeinitFn :: ClapDeinitFn -> IO (FunPtr ClapDeinitFn)

foreign import ccall "wrapper"
  mkGetFactoryFn :: ClapGetFactoryFn -> IO (FunPtr ClapGetFactoryFn) 

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
