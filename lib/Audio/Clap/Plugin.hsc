{-# LANGUAGE RecordWildCards #-}

module Audio.Clap.Plugin where

import Audio.Clap.Version
import Control.Monad (when)
import Foreign
import Foreign.C
import Prelude hiding (id)

#include "clap/plugin.h"

-- | The CLAP plugin descriptor type
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

-- | Create a new CLAP plugin descriptor
newClapPluginDescriptor :: String -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> [String] -> IO (Ptr ClapPluginDescriptor)
newClapPluginDescriptor id name vendor url manualUrl supportUrl version description features = do
  idCString <- newCString id
  nameCString <- newCString name
  vendorCString <- maybe (pure nullPtr) newCString vendor
  urlCString <- maybe (pure nullPtr) newCString url
  manualUrlCString <- maybe (pure nullPtr) newCString manualUrl
  supportUrlCString <- maybe (pure nullPtr) newCString supportUrl
  versionCString <- maybe (pure nullPtr) newCString version
  descriptionCString <- maybe (pure nullPtr) newCString description
  features' <- mapM newCString features
  featuresArray <- mallocArray0 (length features')
  pokeArray0 nullPtr featuresArray features'
  new ClapPluginDescriptor
    { clapVersion = clapVersionInit
    , id = idCString
    , name = nameCString
    , vendor = vendorCString
    , url = urlCString
    , manualUrl = manualUrlCString
    , supportUrl = supportUrlCString
    , version = versionCString
    , description = descriptionCString
    , features = featuresArray
    }

-- | Destroy a CLAP plugin descriptor
destroyClapPluginDescriptor :: Ptr ClapPluginDescriptor -> IO ()
destroyClapPluginDescriptor descriptorPtr = when (descriptorPtr /= nullPtr) $ do
  descriptor <- peek descriptorPtr
  safeFree (id descriptor)
  safeFree (name descriptor)
  safeFree (vendor descriptor)
  safeFree (url descriptor)
  safeFree (manualUrl descriptor)
  safeFree (supportUrl descriptor)
  safeFree (version descriptor)
  safeFree (description descriptor)
  features' <- peekArray0 nullPtr(features descriptor)
  mapM_ safeFree features'
  safeFree (features descriptor)
  free descriptorPtr
 where
  safeFree ptr = when (ptr /= nullPtr) $ free ptr
