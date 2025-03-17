{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Audio.Clap.Factory where

import Audio.Clap.Version
import Foreign
import Foreign.C
import Prelude hiding (init, id)

#include <clap/factory/plugin-factory.h>
#include <clap/plugin.h>

-- | CLAP plugin factory ID, must match the C constant exactly
clapPluginFactoryIdStr :: String
clapPluginFactoryIdStr = #{const_str CLAP_PLUGIN_FACTORY_ID}

-- | Get a CString for the CLAP plugin factory ID
getClapPluginFactoryId :: IO CString
getClapPluginFactoryId = newCString clapPluginFactoryIdStr

-- | Plugin feature constants for descriptors
clapPluginFeatureInstrument :: String
clapPluginFeatureInstrument = #{const_str CLAP_PLUGIN_FEATURE_INSTRUMENT}

clapPluginFeatureStereo :: String
clapPluginFeatureStereo = #{const_str CLAP_PLUGIN_FEATURE_STEREO}

clapPluginFeatureSynthesizer :: String
clapPluginFeatureSynthesizer = #{const_str CLAP_PLUGIN_FEATURE_SYNTHESIZER}

-- CLAP plugin descriptor
data ClapPluginDescriptor = ClapPluginDescriptor
  { clapVersion :: !ClapVersion
  , id :: !CString
  , name :: !CString
  , vendor :: !CString
  , url :: !CString
  , manualUrl :: !CString
  , supportUrl :: !CString
  , version :: !CString
  , description :: !CString
  , features :: !(Ptr CString)
  } deriving (Show)

instance Storable ClapPluginDescriptor where
  sizeOf _ = #{size clap_plugin_descriptor_t}
  alignment _ = #{alignment clap_plugin_descriptor_t}
  peek ptr = do
    clapVersion <- #{peek clap_plugin_descriptor_t, clap_version} ptr
    id <- #{peek clap_plugin_descriptor_t, id} ptr
    name <- #{peek clap_plugin_descriptor_t, name} ptr
    vendor <- #{peek clap_plugin_descriptor_t, vendor} ptr
    url <- #{peek clap_plugin_descriptor_t, url} ptr
    manualUrl <- #{peek clap_plugin_descriptor_t, manual_url} ptr
    supportUrl <- #{peek clap_plugin_descriptor_t, support_url} ptr
    version <- #{peek clap_plugin_descriptor_t, version} ptr
    description <- #{peek clap_plugin_descriptor_t, description} ptr
    features <- #{peek clap_plugin_descriptor_t, features} ptr
    return ClapPluginDescriptor{..}
  poke ptr ClapPluginDescriptor{..} = do
    #{poke clap_plugin_descriptor_t, clap_version} ptr clapVersion
    #{poke clap_plugin_descriptor_t, id} ptr id
    #{poke clap_plugin_descriptor_t, name} ptr name
    #{poke clap_plugin_descriptor_t, vendor} ptr vendor
    #{poke clap_plugin_descriptor_t, url} ptr url
    #{poke clap_plugin_descriptor_t, manual_url} ptr manualUrl
    #{poke clap_plugin_descriptor_t, support_url} ptr supportUrl
    #{poke clap_plugin_descriptor_t, version} ptr version
    #{poke clap_plugin_descriptor_t, description} ptr description
    #{poke clap_plugin_descriptor_t, features} ptr features

-- | Creates a plugin descriptor with NULL-terminated feature list
makePluginDescriptor :: 
     ClapVersion      -- ^ CLAP version
  -> CString          -- ^ Plugin ID
  -> CString          -- ^ Plugin name
  -> CString          -- ^ Vendor
  -> CString          -- ^ URL
  -> CString          -- ^ Manual URL
  -> CString          -- ^ Support URL
  -> CString          -- ^ Version
  -> CString          -- ^ Description
  -> [CString]        -- ^ Features (will be NULL-terminated automatically)
  -> IO (Ptr ClapPluginDescriptor)
makePluginDescriptor clapV pluginId name vendor url manualUrl supportUrl version desc features = do
  -- Create NULL-terminated features array
  featuresPtr <- mallocArray (length features + 1)
  pokeArray featuresPtr (features ++ [nullPtr])
  
  -- Allocate and initialize the descriptor
  descriptorPtr <- malloc
  poke descriptorPtr ClapPluginDescriptor
    { clapVersion = clapV
    , id = pluginId
    , name = name
    , vendor = vendor
    , url = url
    , manualUrl = manualUrl
    , supportUrl = supportUrl
    , version = version
    , description = desc
    , features = featuresPtr
    }
  
  return descriptorPtr

-- | The CLAP plugin factory type
data ClapPluginFactory = ClapPluginFactory
  { getPluginCount :: !(FunPtr (Ptr ClapPluginFactory -> IO CUInt))
  , getPluginDescriptor :: !(FunPtr (Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)))
  , createPlugin :: !(FunPtr (Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ())))
  } deriving (Show)

instance Storable ClapPluginFactory where
  sizeOf _ = #{size clap_plugin_factory_t}
  alignment _ = #{alignment clap_plugin_factory_t}
  peek ptr = do
    getPluginCount <- #{peek clap_plugin_factory_t, get_plugin_count} ptr
    getPluginDescriptor <- #{peek clap_plugin_factory_t, get_plugin_descriptor} ptr
    createPlugin <- #{peek clap_plugin_factory_t, create_plugin} ptr
    return ClapPluginFactory{..}
  poke ptr ClapPluginFactory{..} = do
    #{poke clap_plugin_factory_t, get_plugin_count} ptr getPluginCount
    #{poke clap_plugin_factory_t, get_plugin_descriptor} ptr getPluginDescriptor
    #{poke clap_plugin_factory_t, create_plugin} ptr createPlugin

-- | Create function pointers for factory functions
foreign import ccall "wrapper"
  mkGetPluginCountFunPtr :: (Ptr ClapPluginFactory -> IO CUInt) -> IO (FunPtr (Ptr ClapPluginFactory -> IO CUInt))

foreign import ccall "wrapper"
  mkGetPluginDescriptorFunPtr :: (Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)) -> IO (FunPtr (Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)))

foreign import ccall "wrapper"
  mkCreatePluginFunPtr :: (Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ())) -> IO (FunPtr (Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ())))

-- | Creates a plugin factory with the given implementation functions
makePluginFactory :: 
     (Ptr ClapPluginFactory -> IO CUInt)                              -- ^ get_plugin_count implementation
  -> (Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)) -- ^ get_plugin_descriptor implementation
  -> (Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ()))       -- ^ create_plugin implementation
  -> IO (Ptr ClapPluginFactory)
makePluginFactory countFn descriptorFn createFn = do
  -- Create function pointers
  countPtr <- mkGetPluginCountFunPtr countFn
  descriptorPtr <- mkGetPluginDescriptorFunPtr descriptorFn
  createPtr <- mkCreatePluginFunPtr createFn
  
  -- Allocate and initialize the factory
  factoryPtr <- malloc
  poke factoryPtr ClapPluginFactory
    { getPluginCount = countPtr
    , getPluginDescriptor = descriptorPtr
    , createPlugin = createPtr
    }
  
  return factoryPtr 