{-# LANGUAGE StrictData #-}

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

initPlugin :: IO Plugin
initPlugin =
  Plugin
    <$> newIORef 0
    <*> newPluginFactory pluginCount descriptor createPlugin
    <*> newClapPluginDescriptor
      "hsynth.blue.bfly"
      "hsynth"
      (Just "bflyblue")
      (Just "https://github.com/hsynth")
      Nothing
      Nothing
      (Just "0.0")
      (Just "Haskell Synth")
      []

{-# NOINLINE plugin #-}
plugin :: Plugin
plugin = unsafePerformIO initPlugin

pluginCount :: GetPluginCount
pluginCount _ = do
  pure 1

descriptor :: GetPluginDescriptor
descriptor _ 0 = pure (pluginDescriptor plugin)
descriptor _ _ = pure nullPtr

createPlugin :: CreatePlugin
createPlugin _ _host _cPluginId = pure nullPtr

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