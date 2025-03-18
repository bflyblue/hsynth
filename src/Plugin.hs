module Plugin where

import Audio.Clap.Factory.PluginFactory
import Audio.Clap.Plugin
import Data.IORef
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

data Plugin = Plugin
  { initCounter :: IORef Int
  , pluginFactory :: Ptr ClapPluginFactory
  , pluginDescriptor :: Ptr ClapPluginDescriptor
  }

plugin :: Plugin
plugin =
  Plugin
    { initCounter = unsafePerformIO $ newIORef 0
    , pluginFactory = unsafePerformIO $ newPluginFactory pluginCount descriptor createPlugin
    , pluginDescriptor = unsafePerformIO $ newClapPluginDescriptor "hsynth" "hsynth" (Just "https://github.com/hsynth") (Just "https://github.com/hsynth") (Just "https://github.com/hsynth") (Just "https://github.com/hsynth") (Just "https://github.com/hsynth") (Just "https://github.com/hsynth") []
    }

pluginCount :: GetPluginCount
pluginCount _ = do
  return 1

descriptor :: GetPluginDescriptor
descriptor _ index = do
  case index of
    0 -> do
      return (pluginDescriptor plugin)
    _ -> do
      return nullPtr

createPlugin :: CreatePlugin
createPlugin _ host cPluginId = do
  return nullPtr

foreign export ccall "pluginInit" pluginInit :: CString -> IO CBool

pluginInit :: CString -> IO CBool
pluginInit _pluginPath = do
  count <- atomicModifyIORef' (initCounter plugin) (\x -> (x + 1, x))
  if count == 0
    then do
      putStrLn "Plugin initializing..."
      pure 1
    else do
      putStrLn "Plugin already initialized"
      pure 1

foreign export ccall "pluginDeinit" pluginDeinit :: IO ()

pluginDeinit :: IO ()
pluginDeinit = do
  count <- atomicModifyIORef' (initCounter plugin) (\x -> (x - 1, x - 1))
  if count == 0
    then do
      putStrLn "Plugin deinitializing..."
    else do
      putStrLn "Plugin already deinitialized"

foreign export ccall "pluginGetFactory" pluginGetFactory :: CString -> IO (Ptr ())

pluginGetFactory :: CString -> IO (Ptr ())
pluginGetFactory cFactoryId = do
  factoryId <- peekCString cFactoryId
  case factoryId of
    CLAP_PLUGIN_FACTORY_ID -> do
      pure (castPtr (pluginFactory plugin))
    _ -> do
      pure nullPtr