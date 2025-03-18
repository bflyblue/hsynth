{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Audio.Clap.Factory.PluginFactory where

import Audio.Clap.Plugin
import Control.Monad (when)
import Foreign
import Foreign.C

#include <clap/factory/plugin-factory.h>

pattern CLAP_PLUGIN_FACTORY_ID :: String
pattern CLAP_PLUGIN_FACTORY_ID = (#const_str CLAP_PLUGIN_FACTORY_ID)

-- | The CLAP plugin factory type
data ClapPluginFactory = ClapPluginFactory
  { getPluginCountFunPtr :: !(FunPtr GetPluginCount)
  -- ^ Get the number of plugins in the factory
  -- [thread-safe]
  , getPluginDescriptorFunPtr :: !(FunPtr GetPluginDescriptor)
  -- ^ Get a plugin descriptor by it's index
  -- Returns null in case of error
  -- The descriptor must not be freed
  -- [thread-safe]
  , createPluginFunPtr :: !(FunPtr CreatePlugin)
  -- ^ Create a plugin by its plugin_id
  -- The returned pointer must be freed by calling plugin->destroy(plugin)
  -- The plugin not allowed to use the host callbacks in the create method.
  -- Returns null in case of error
  -- [thread-safe]
  } deriving (Show)

instance Storable ClapPluginFactory where
  sizeOf _ = #{size clap_plugin_factory_t}
  alignment _ = #{alignment clap_plugin_factory_t}
  peek ptr = do
    getPluginCountFunPtr <- #{peek clap_plugin_factory_t, get_plugin_count} ptr
    getPluginDescriptorFunPtr <- #{peek clap_plugin_factory_t, get_plugin_descriptor} ptr
    createPluginFunPtr <- #{peek clap_plugin_factory_t, create_plugin} ptr
    return ClapPluginFactory{..}
  poke ptr ClapPluginFactory{..} = do
    #{poke clap_plugin_factory_t, get_plugin_count} ptr getPluginCountFunPtr
    #{poke clap_plugin_factory_t, get_plugin_descriptor} ptr getPluginDescriptorFunPtr
    #{poke clap_plugin_factory_t, create_plugin} ptr createPluginFunPtr

type GetPluginCount = Ptr ClapPluginFactory -> IO CUInt
type GetPluginDescriptor = Ptr ClapPluginFactory -> CUInt -> IO (Ptr ClapPluginDescriptor)
type CreatePlugin = Ptr ClapPluginFactory -> Ptr () -> CString -> IO (Ptr ())

foreign import ccall "wrapper" mkGetPluginCount :: GetPluginCount -> IO (FunPtr GetPluginCount)
foreign import ccall "wrapper" mkGetPluginDescriptor :: GetPluginDescriptor -> IO (FunPtr GetPluginDescriptor)
foreign import ccall "wrapper" mkCreatePlugin :: CreatePlugin -> IO (FunPtr CreatePlugin)

newPluginFactory :: GetPluginCount -> GetPluginDescriptor -> CreatePlugin -> IO (Ptr ClapPluginFactory)
newPluginFactory getPluginCount' getPluginDescriptor' createPlugin' = do
  new =<< ClapPluginFactory
    <$> mkGetPluginCount getPluginCount'
    <*> mkGetPluginDescriptor getPluginDescriptor'
    <*> mkCreatePlugin createPlugin'

destroyPluginFactory :: Ptr ClapPluginFactory -> IO ()
destroyPluginFactory factoryPtr = when (factoryPtr /= nullPtr) $ do
  factory <- peek factoryPtr  
  freeHaskellFunPtr (getPluginCountFunPtr factory)
  freeHaskellFunPtr (getPluginDescriptorFunPtr factory)
  freeHaskellFunPtr (createPluginFunPtr factory)
  free factoryPtr
