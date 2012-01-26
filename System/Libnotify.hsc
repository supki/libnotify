{-# LANGUAGE ForeignFunctionInterface #-}
-- Module  --{{{1

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




-- Notify - Notification API  --{{{1
-- Data and Types  ---{{{2

data ServerInfo = ServerInfo
  { serverName  :: String
  , serverVender :: String
  , serverVersion :: String
  , serverSpecVersion :: String
  } deriving (Eq, Ord, Read, Show)




initNotify :: String -> IO Bool  --{{{2
initNotify appName =
  withCString appName $ \p_appName ->
  notify_init p_appName

foreign import ccall unsafe "libnotify/notify.h notify_init"
  notify_init :: CString -> IO Bool





uninitNotify :: IO ()  --{{{2
uninitNotify = notify_uninit

foreign import ccall unsafe "libnotify/notify.h notify_uninit"
  notify_uninit :: IO ()





isInitted :: IO Bool  --{{{2
isInitted = notify_is_initted

foreign import ccall unsafe "libnotify/notify.h notify_is_initted"
  notify_is_initted :: IO Bool




getAppName :: IO String  --{{{2
getAppName = do
  p_appName <- notify_get_app_name
  peekCString p_appName

foreign import ccall unsafe "libnotify/notify.h notify_get_app_name"
  notify_get_app_name :: IO CString




setAppName :: String -> IO ()  --{{{2
setAppName appName =
  withCString appName $ \p_appName ->
  notify_set_app_name p_appName

foreign import ccall unsafe "libnotify/notify.h notify_set_app_name"
  notify_set_app_name :: CString -> IO ()




getServerCaps :: IO [String]  --{{{2
getServerCaps = do
  p_caps <- notify_get_server_caps >>= readGList
  mapM peekCString p_caps

foreign import ccall unsafe "libnotify/notify.h notify_get_server_caps"
  notify_get_server_caps :: IO GList




getServerInfo :: IO ServerInfo  --{{{2
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




-- Notification - A passive pop-up notification  --{{{1
-- Data and Types  ---{{{2

newtype Notification = Notification (Ptr Notification)

newtype NotificationTimeout = NotificationTimeout {getTimeout :: CInt}

newtype Urgency = Urgency CInt
#{enum Urgency, Urgency,
  notifyUrgencyLow      = NOTIFY_URGENCY_LOW,
  notifyUrgencyNormal   = NOTIFY_URGENCY_NORMAL,
  notifyUrgencyCritical = NOTIFY_URGENCY_CRITICAL
}

type ActionCallback a = Notification -> CString -> Ptr a -> IO ()
type FreeFunc a = Ptr a -> IO ()




newNotify :: String -> Maybe String -> Maybe String -> IO (Notification)  --{{{2
newNotify summary body icon =
  withCString summary        $ \p_summary ->
  maybeWith withCString body $ \p_body ->
  maybeWith withCString icon $ \p_icon ->
  notify_notification_new p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_new"
  notify_notification_new :: CString
                          -> CString
                          -> CString
                          -> IO (Notification)




updateNotify :: Notification -> String -> Maybe String -> Maybe String -> IO Bool  --{{{2
updateNotify notify summary body icon =
  withCString summary        $ \p_summary ->
  maybeWith withCString body $ \p_body ->
  maybeWith withCString icon $ \p_icon ->
  notify_notification_update notify p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_update"
  notify_notification_update :: Notification
                             -> CString
                             -> CString
                             -> CString -> IO Bool




showNotify :: Notification -> IO Bool  --{{{2
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
  notify_notification_show :: Notification -> Ptr (Ptr GError) -> IO Bool

foreign import ccall unsafe "glib-object.h g_error_free"
  g_error_free :: Ptr GError -> IO ()




expiresDefault :: NotificationTimeout --{{{2
expiresDefault = NotificationTimeout $ #const NOTIFY_EXPIRES_DEFAULT

expiresNever :: NotificationTimeout
expiresNever = NotificationTimeout #const NOTIFY_EXPIRES_NEVER

expires :: Int -> NotificationTimeout
expires = NotificationTimeout . fromIntegral

setTimeout :: Notification -> NotificationTimeout -> IO ()  --{{{2
setTimeout notify timeout =
  notify_notification_set_timeout notify (getTimeout timeout)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout :: Notification -> CInt -> IO ()




setCategory :: Notification -> String -> IO ()  --{{{2
setCategory notify category =
  withCString category $ \p_category ->
  notify_notification_set_category notify p_category

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category :: Notification -> CString -> IO ()




setUrgency :: Notification -> Urgency -> IO ()  --{{{2
setUrgency notify (Urgency urgency) =
  notify_notification_set_urgency notify urgency

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_urgency"
  notify_notification_set_urgency :: Notification -> CInt -> IO ()




setIconFromPixbuf :: Notification -> Pixbuf -> IO ()  --{{{2
setIconFromPixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_icon_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_icon_from_pixbuf"
  notify_notification_set_icon_from_pixbuf :: Notification
                                           -> Ptr Pixbuf
                                           -> IO ()




setImageFromPixbuf :: Notification -> Pixbuf -> IO ()  --{{{2
setImageFromPixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_image_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_image_from_pixbuf"
  notify_notification_set_image_from_pixbuf :: Notification
                                            -> Ptr Pixbuf
                                            -> IO ()




setHintInt32 :: Notification -> String -> Int32 -> IO ()  --{{{2
setHintInt32 notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_int32 notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_int32"
  notify_notification_set_hint_int32 :: Notification
                                     -> CString
                                     -> CInt
                                     -> IO ()




setHintDouble :: Notification -> String -> Double -> IO ()  --{{{2
setHintDouble notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_double notify p_key (realToFrac value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_double"
  notify_notification_set_hint_double :: Notification
                                      -> CString
                                      -> CDouble
                                      -> IO ()


setHintString :: Notification -> String -> String -> IO ()  --{{{2
setHintString notify key value =
  withCString key   $ \p_key ->
  withCString value $ \p_value ->
  notify_notification_set_hint_string notify p_key p_value

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_string"
  notify_notification_set_hint_string :: Notification
                                      -> CString
                                      -> CString
                                      -> IO ()




setHintByte :: Notification -> String -> Word8 -> IO ()  --{{{2
setHintByte notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_byte notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte"
  notify_notification_set_hint_byte :: Notification
                                    -> CString
                                    -> CUChar
                                    -> IO ()




setHintByteArray :: Notification -> String -> BS.ByteString -> IO ()  --{{{2
setHintByteArray notify key value =
  withCString key $ \p_key ->
  withArrayLen (BS.foldr' step [] value) $ \len p_bs ->
  notify_notification_set_hint_byte_array notify p_key p_bs (fromIntegral len)
    where
      step x xs = fromIntegral x:xs

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte_array"
  notify_notification_set_hint_byte_array :: Notification
                                          -> CString
                                          -> Ptr CUChar
                                          -> CSize
                                          -> IO ()




clearHints :: Notification -> IO ()  --{{{2
clearHints = notify_notification_clear_hints

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_hints"
  notify_notification_clear_hints :: Notification -> IO ()




addAction  --{{{2
  :: Notification
  -> String
  -> String
  -> (Notification -> String -> IO ())
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
  notify_notification_add_action :: Notification
                                 -> CString
                                 -> CString
                                 -> FunPtr (ActionCallback a)
                                 -> Ptr a
                                 -> FunPtr (FreeFunc a)
                                 -> IO ()




clearActions :: Notification -> IO ()  --{{{2
clearActions = notify_notification_clear_actions

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_actions"
  notify_notification_clear_actions :: Notification -> IO ()




closeNotify :: Notification -> IO Bool  --{{{2
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
  notify_notification_close :: Notification -> Ptr (Ptr GError) -> IO Bool




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
