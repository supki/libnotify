{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Low level bindings to libnotify
--
-- See also <https://developer.gnome.org/libnotify/0.7/libnotify-notify.html>. Haddocks here
-- are mostly excerpts from there
module Libnotify.C.Notify
  ( notify_init
  , notify_uninit
  , notify_is_initted
  , notify_get_app_name
  , notify_set_app_name
  , notify_get_server_caps
  , ServerInfo(..)
  , notify_get_server_info
  ) where

#include <libnotify/notify.h>

import Data.Data (Typeable, Data)
import GHC.Generics (Generic)
import Foreign
import Foreign.C
import System.Glib.GList (GList, readGList)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | Initialize libnotify
--
-- This must be called before any other functions
notify_init
  :: String -- ^ Application name. Should not be empty!
  -> IO Bool
notify_init name = withCString name notify_init_c

-- | Uninitialize libnotify
notify_uninit :: IO ()
notify_uninit = notify_uninit_c

-- | Get whether libnotify is initialized or not
notify_is_initted :: IO Bool
notify_is_initted = notify_is_initted_c

-- | Get the application name
--
-- Do not forget to call 'notify_init' before calling this!
notify_get_app_name :: IO String
notify_get_app_name = notify_get_app_name_c >>= peekCString

-- | Set the application name
--
-- Do not forget to call 'notify_init' before calling this!
notify_set_app_name :: String -> IO ()
notify_set_app_name name = withCString name notify_set_app_name_c

-- | Return server capabilities
--
-- Synchronously queries the server for its capabilities
--
-- >>> notify_get_server_caps
-- ["actions","body","body-markup","body-hyperlinks","icon-static","x-canonical-private-icon-only"]
notify_get_server_caps :: IO [String]
notify_get_server_caps = do
  glist  <- notify_get_server_caps_c
  p_caps <- readGList glist
  caps   <- mapM peekCString p_caps
  mapM_ free p_caps -- free list elements
  g_list_free glist -- free list itself
  return caps

-- | Server information
data ServerInfo = ServerInfo
  { serverName        :: String
  , serverVendor      :: String
  , serverVersion     :: String
  , serverSpecVersion :: String
  } deriving (Show, Eq, Typeable, Data, Generic)

-- | Return server information
--
-- Synchronously queries the server for its information, specifically,
-- the name, vendor, server version, and the version of the notifications
-- specification that it is compliant with
--
-- >>> notify_get_server_info
-- Just (ServerInfo {name = "Xfce Notify Daemon", vendor = "Xfce", version = "0.2.4", specVersion = "0.9"})
notify_get_server_info :: IO (Maybe ServerInfo)
notify_get_server_info =
  alloca $ \p_name ->
  alloca $ \p_vendor ->
  alloca $ \p_version ->
  alloca $ \p_specVersion -> do
    ret <- notify_get_server_info_c p_name p_vendor p_version p_specVersion
    if ret
      then do
        serverName        <- peekCString =<< peek p_name
        serverVendor      <- peekCString =<< peek p_vendor
        serverVersion     <- peekCString =<< peek p_version
        serverSpecVersion <- peekCString =<< peek p_specVersion
        return $ Just ServerInfo {..}
      else
        return Nothing

foreign import ccall safe "libnotify/notify.h notify_init"
  notify_init_c :: CString -> IO Bool

foreign import ccall safe "libnotify/notify.h notify_uninit"
  notify_uninit_c :: IO ()

foreign import ccall safe "libnotify/notify.h notify_is_initted"
  notify_is_initted_c :: IO Bool

foreign import ccall safe "libnotify/notify.h notify_get_app_name"
  notify_get_app_name_c :: IO CString

foreign import ccall safe "libnotify/notify.h notify_set_app_name"
  notify_set_app_name_c :: CString -> IO ()

foreign import ccall safe "libnotify/notify.h notify_get_server_caps"
  notify_get_server_caps_c :: IO GList

foreign import ccall safe "libnotify/notify.h notify_get_server_info"
  notify_get_server_info_c :: Ptr CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO Bool

foreign import ccall safe "g_list_free"
  g_list_free :: GList -> IO ()
