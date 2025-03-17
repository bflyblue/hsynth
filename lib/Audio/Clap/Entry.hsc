{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

module Audio.Clap.Entry where

import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Prelude hiding (init)

#include <clap/clap.h>

-- | The CLAP Version structure
data ClapVersion = ClapVersion
  { major :: !Word32
  , minor :: !Word32
  , revision :: !Word32
  } deriving (Eq, Show)

instance Storable ClapVersion where
  sizeOf _ = #{size clap_version_t}
  alignment _ = #{alignment clap_version_t}
  peek ptr = do
    major <- #{peek clap_version_t, major} ptr
    minor <- #{peek clap_version_t, minor} ptr
    revision <- #{peek clap_version_t, revision} ptr
    return ClapVersion{..}
  poke ptr ClapVersion{..} = do
    #{poke clap_version_t, major} ptr major
    #{poke clap_version_t, minor} ptr minor
    #{poke clap_version_t, revision} ptr revision

-- Current CLAP version constants
clapVersionMajor :: Word32
clapVersionMajor = #{const CLAP_VERSION_MAJOR}

clapVersionMinor :: Word32
clapVersionMinor = #{const CLAP_VERSION_MINOR}

clapVersionRevision :: Word32
clapVersionRevision = #{const CLAP_VERSION_REVISION}

-- | Create a ClapVersion with the current CLAP version
clapCurrentVersion :: ClapVersion
clapCurrentVersion = ClapVersion 
  { major = clapVersionMajor
  , minor = clapVersionMinor
  , revision = clapVersionRevision
  }

-- | Check if a version is compatible with the current CLAP version
clapVersionIsCompatible :: ClapVersion -> Bool
clapVersionIsCompatible ClapVersion{..} = major >= 1

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