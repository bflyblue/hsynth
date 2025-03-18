{-# LANGUAGE RecordWildCards #-}

module Audio.Clap.Version where

import Foreign

#include <clap/version.h>

-- | The CLAP Version
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

-- | Initialize a ClapVersion with the current CLAP version
clapVersionInit :: ClapVersion
clapVersionInit = ClapVersion 
  { major = clapVersionMajor
  , minor = clapVersionMinor
  , revision = clapVersionRevision
  }

-- | Check if a version is compatible with the current CLAP version
clapVersionIsCompatible :: ClapVersion -> Bool
clapVersionIsCompatible ClapVersion{..} = major >= 1 