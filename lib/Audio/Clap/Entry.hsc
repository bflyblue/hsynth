{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Audio.Clap.Entry where

import Audio.Clap.Version
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
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

-- Foreign function to get the entry point
foreign import ccall "&clap_entry"
  clapEntry :: Ptr ClapPluginEntry

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