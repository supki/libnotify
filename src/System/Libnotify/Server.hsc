{-# LANGUAGE ForeignFunctionInterface #-}
-- | System.Libnotify.Server module deals with notification server info processing and
-- initialized applications managing.
{-# OPTIONS_HADDOCK prune #-}

#include <libnotify/notify.h>

module System.Libnotify.Server
  ( getAppName, setAppName, getServerCaps, getServerInfo
  ) where

import Foreign
import Foreign.C
import System.Glib.GList

import System.Libnotify.Types (ServerInfo(..))

-- | Returns registered application name.
getAppName :: IO String
getAppName = do
  p_appName <- notify_get_app_name
  peekCString p_appName

foreign import ccall unsafe "libnotify/notify.h notify_get_app_name"
  notify_get_app_name :: IO CString

-- | Updates registered application name.
setAppName :: String -> IO ()
setAppName appName =
  withCString appName $ \p_appName ->
  notify_set_app_name p_appName

foreign import ccall unsafe "libnotify/notify.h notify_set_app_name"
  notify_set_app_name :: CString -> IO ()

-- | Returns server capability strings.
getServerCaps :: IO [String]
getServerCaps = do
  g_type_init
  p_caps <- notify_get_server_caps >>= readGList
  mapM peekCString p_caps

foreign import ccall unsafe "libnotify/notify.h notify_get_server_caps"
  notify_get_server_caps :: IO GList

-- | Returns server information.
getServerInfo :: IO ServerInfo
getServerInfo =
  alloca $ \p_name ->
  alloca $ \p_vendor ->
  alloca $ \p_version ->
  alloca $ \p_specVersion -> do
    g_type_init
    notify_get_server_info p_name p_vendor p_version p_specVersion
    name        <- peekCString =<< peek p_name
    vendor      <- peekCString =<< peek p_vendor
    version     <- peekCString =<< peek p_version
    specVersion <- peekCString =<< peek p_specVersion
    return $ ServerInfo
      { serverName        = name
      , serverVendor      = vendor
      , serverVersion     = version
      , serverSpecVersion = specVersion
      }

foreign import ccall unsafe "libnotify/notify.h notify_get_server_info"
  notify_get_server_info :: (Ptr CString)
                         -> (Ptr CString)
                         -> (Ptr CString)
                         -> (Ptr CString)
                         -> IO Bool

foreign import ccall safe "g_type_init"
  g_type_init :: IO ()

