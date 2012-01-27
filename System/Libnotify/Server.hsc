{-# LANGUAGE ForeignFunctionInterface #-}

#include <libnotify/notify.h>

module System.Libnotify.Server
  ( ServerInfo(..)
  , getAppName, setAppName, getServerCaps, getServerInfo
  ) where

import Foreign
import Foreign.C
import System.Glib.GList

data ServerInfo = ServerInfo
  { serverName  :: String
  , serverVender :: String
  , serverVersion :: String
  , serverSpecVersion :: String
  } deriving (Eq, Ord, Read, Show)

getAppName :: IO String
getAppName = do
  p_appName <- notify_get_app_name
  peekCString p_appName

foreign import ccall unsafe "libnotify/notify.h notify_get_app_name"
  notify_get_app_name :: IO CString

setAppName :: String -> IO ()
setAppName appName =
  withCString appName $ \p_appName ->
  notify_set_app_name p_appName

foreign import ccall unsafe "libnotify/notify.h notify_set_app_name"
  notify_set_app_name :: CString -> IO ()

getServerCaps :: IO [String]
getServerCaps = do
  p_caps <- notify_get_server_caps >>= readGList
  mapM peekCString p_caps

foreign import ccall unsafe "libnotify/notify.h notify_get_server_caps"
  notify_get_server_caps :: IO GList

getServerInfo :: IO ServerInfo
getServerInfo =
  alloca $ \p_name ->
  alloca $ \p_vender ->
  alloca $ \p_version ->
  alloca $ \p_specVersion -> do
    notify_get_server_info p_name p_vender p_version p_specVersion
    name        <- peekCString =<< peek p_name
    vender      <- peekCString =<< peek p_vender
    version     <- peekCString =<< peek p_version
    specVersion <- peekCString =<< peek p_specVersion
    return $ ServerInfo
      { serverName        = name
      , serverVender      = vender
      , serverVersion     = version
      , serverSpecVersion = specVersion
      }

foreign import ccall unsafe "libnotify/notify.h notify_get_server_info"
  notify_get_server_info :: (Ptr CString)
                         -> (Ptr CString)
                         -> (Ptr CString)
                         -> (Ptr CString)
                         -> IO Bool

