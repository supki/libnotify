{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

#include <libnotify/notify.h>

module System.Libnotify (
  ServerInfo(..),
  initNotify, uninitNotify, isInitted,
  getAppName, setAppName, getServerCaps, getServerInfo,

  Notification, NotificationTimeout, Urgency,
  newNotify, updateNotify, showNotify,
  setTimeout, expiresDefault, expiresNever, expires,
  setCategory, setUrgency, setIconFromPixbuf, setImageFromPixbuf,
  setHintInt32, setHintDouble, setHintString, setHintByte, setHintByteArray,
  addAction, clearActions, closeNotify
) where

import Control.Applicative
import Control.Exception
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C
import Graphics.UI.Gtk.Gdk.Pixbuf
import System.Glib.GError
import System.Glib.GList
import Unsafe.Coerce

import qualified Data.ByteString as BS

data ServerInfo = ServerInfo
  { serverName  :: String
  , serverVender :: String
  , serverVersion :: String
  , serverSpecVersion :: String
  } deriving (Eq, Ord, Read, Show)

initNotify :: String -> IO Bool
initNotify appName =
  withCString appName $ \p_appName ->
  notify_init p_appName

foreign import ccall unsafe "libnotify/notify.h notify_init"
  notify_init :: CString -> IO Bool

uninitNotify :: IO ()
uninitNotify = notify_uninit

foreign import ccall unsafe "libnotify/notify.h notify_uninit"
  notify_uninit :: IO ()

isInitted :: IO Bool
isInitted = notify_is_initted

foreign import ccall unsafe "libnotify/notify.h notify_is_initted"
  notify_is_initted :: IO Bool

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

data Notification

newtype NotificationTimeout = NotificationTimeout {getTimeout :: CInt}

newtype Urgency = Urgency CInt
#{enum Urgency, Urgency,
  notifyUrgencyLow      = NOTIFY_URGENCY_LOW,
  notifyUrgencyNormal   = NOTIFY_URGENCY_NORMAL,
  notifyUrgencyCritical = NOTIFY_URGENCY_CRITICAL
}

type ActionCallback a = Ptr Notification -> CString -> Ptr a -> IO ()
type FreeFunc a = Ptr a -> IO ()

newNotify :: String -> Maybe String -> Maybe String -> IO (Ptr Notification)
newNotify summary body icon =
  withCString summary        $ \p_summary ->
  maybeWith withCString body $ \p_body ->
  maybeWith withCString icon $ \p_icon ->
  notify_notification_new p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_new"
  notify_notification_new :: CString
                          -> CString
                          -> CString
                          -> IO (Ptr Notification)

updateNotify :: Ptr Notification -> String -> Maybe String -> Maybe String -> IO Bool
updateNotify notify summary body icon =
  withCString summary        $ \p_summary ->
  maybeWith withCString body $ \p_body ->
  maybeWith withCString icon $ \p_icon ->
  notify_notification_update notify p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_update"
  notify_notification_update :: Ptr Notification
                             -> CString
                             -> CString
                             -> CString -> IO Bool

showNotify :: Ptr Notification -> IO Bool
showNotify notify =
  alloca $ \pp_error -> do
  poke pp_error nullPtr
  result <- notify_notification_show notify pp_error
  p_error <- peek pp_error
  if p_error == nullPtr
    then return result
    else do error <- peek p_error
            g_error_free p_error
            throwGError error

foreign import ccall unsafe "libnotify/notify.h notify_notification_show"
  notify_notification_show :: Ptr Notification -> Ptr (Ptr GError) -> IO Bool

foreign import ccall unsafe "glib-object.h g_error_free"
  g_error_free :: Ptr GError -> IO ()

expiresDefault :: NotificationTimeout
expiresDefault = NotificationTimeout $ #const NOTIFY_EXPIRES_DEFAULT

expiresNever :: NotificationTimeout
expiresNever = NotificationTimeout #const NOTIFY_EXPIRES_NEVER

expires :: Int -> NotificationTimeout
expires = NotificationTimeout . fromIntegral

setTimeout :: Ptr Notification -> NotificationTimeout -> IO ()
setTimeout notify timeout =
  notify_notification_set_timeout notify (getTimeout timeout)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout :: Ptr Notification -> CInt -> IO ()

setCategory :: Ptr Notification -> String -> IO ()
setCategory notify category =
  withCString category $ \p_category ->
  notify_notification_set_category notify p_category

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category :: Ptr Notification -> CString -> IO ()

setUrgency :: Ptr Notification -> Urgency -> IO ()
setUrgency notify (Urgency urgency) =
  notify_notification_set_urgency notify urgency

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_urgency"
  notify_notification_set_urgency :: Ptr Notification -> CInt -> IO ()

setIconFromPixbuf :: Ptr Notification -> Pixbuf -> IO ()
setIconFromPixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_icon_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_icon_from_pixbuf"
  notify_notification_set_icon_from_pixbuf :: Ptr Notification
                                           -> Ptr Pixbuf
                                           -> IO ()

setImageFromPixbuf :: Ptr Notification -> Pixbuf -> IO ()
setImageFromPixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_image_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_image_from_pixbuf"
  notify_notification_set_image_from_pixbuf :: Ptr Notification
                                            -> Ptr Pixbuf
                                            -> IO ()

setHintInt32 :: Ptr Notification -> String -> Int32 -> IO ()
setHintInt32 notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_int32 notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_int32"
  notify_notification_set_hint_int32 :: Ptr Notification
                                     -> CString
                                     -> CInt
                                     -> IO ()

setHintDouble :: Ptr Notification -> String -> Double -> IO ()
setHintDouble notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_double notify p_key (realToFrac value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_double"
  notify_notification_set_hint_double :: Ptr Notification
                                      -> CString
                                      -> CDouble
                                      -> IO ()

setHintString :: Ptr Notification -> String -> String -> IO ()
setHintString notify key value =
  withCString key   $ \p_key ->
  withCString value $ \p_value ->
  notify_notification_set_hint_string notify p_key p_value

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_string"
  notify_notification_set_hint_string :: Ptr Notification
                                      -> CString
                                      -> CString
                                      -> IO ()

setHintByte :: Ptr Notification -> String -> Word8 -> IO ()
setHintByte notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_byte notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte"
  notify_notification_set_hint_byte :: Ptr Notification
                                    -> CString
                                    -> CUChar
                                    -> IO ()

setHintByteArray :: Ptr Notification -> String -> BS.ByteString -> IO ()
setHintByteArray notify key value =
  withCString key $ \p_key ->
  withArrayLen (BS.foldr' step [] value) $ \len p_bs ->
  notify_notification_set_hint_byte_array notify p_key p_bs (fromIntegral len)
    where
      step x xs = fromIntegral x:xs

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte_array"
  notify_notification_set_hint_byte_array :: Ptr Notification
                                          -> CString
                                          -> Ptr CUChar
                                          -> CSize
                                          -> IO ()

clearHints :: Ptr Notification -> IO ()
clearHints = notify_notification_clear_hints

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_hints"
  notify_notification_clear_hints :: Ptr Notification -> IO ()

addAction
  :: Ptr Notification
  -> String
  -> String
  -> (Ptr Notification -> String -> IO ())
  -> IO ()
addAction notify action label callback =
  withCString action $ \p_action ->
  withCString label  $ \p_label -> do
    p_callback <- wrapActionCallback $ makeCallback callback
    notify_notification_add_action notify
                                   p_action
                                   p_label
                                   p_callback
                                   nullPtr
                                   nullFunPtr
  where
    makeCallback callback = \notify p_action _ -> do
      action <- peekCString p_action
      callback notify action

foreign import ccall "wrapper"
  wrapActionCallback :: (ActionCallback a) -> IO (FunPtr (ActionCallback a))

foreign import ccall "wrapper"
  wrapFreeFunc :: (FreeFunc a) -> IO (FunPtr (FreeFunc a))

foreign import ccall unsafe "libnotify/notify.h notify_notification_add_action"
  notify_notification_add_action :: Ptr Notification
                                 -> CString
                                 -> CString
                                 -> FunPtr (ActionCallback a)
                                 -> Ptr a
                                 -> FunPtr (FreeFunc a)
                                 -> IO ()

clearActions :: Ptr Notification -> IO ()
clearActions = notify_notification_clear_actions

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_actions"
  notify_notification_clear_actions :: Ptr Notification -> IO ()

closeNotify :: Ptr Notification -> IO Bool
closeNotify notify =
  alloca $ \pp_error -> do
  poke pp_error nullPtr
  result <- notify_notification_close notify pp_error
  p_error <- peek pp_error
  if p_error == nullPtr
    then return result
    else do error <- peek p_error
            g_error_free p_error
            throwGError error

foreign import ccall unsafe "libnotify/notify.h notify_notification_close"
  notify_notification_close :: Ptr Notification -> Ptr (Ptr GError) -> IO Bool
