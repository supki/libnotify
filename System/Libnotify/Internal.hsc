{-# LANGUAGE ForeignFunctionInterface #-}

#include <libnotify/notify.h>

module System.Libnotify.Internal
  ( initNotify, uninitNotify, isInitted
  , newNotify, updateNotify, showNotify
  , setTimeout, expiresDefault, expiresNever, expires
  , setCategory, setUrgency, setIconFromPixbuf, setImageFromPixbuf
  , setHintInt32, setHintDouble, setHintString, setHintByte, setHintByteArray, clearHints
  , addAction, clearActions, closeNotify
  ) where

import Foreign
import Foreign.C
import Graphics.UI.Gtk.Gdk.Pixbuf
import System.Glib.GError
import System.Glib.GList
import Unsafe.Coerce
import qualified Data.ByteString as BS

import System.Libnotify.Types

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

setTimeout :: Ptr Notification -> Timeout -> IO ()
setTimeout notify timeout =
  notify_notification_set_timeout notify (getTimeout timeout)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout :: Ptr Notification -> CInt -> IO ()

setCategory :: Ptr Notification -> Category -> IO ()
setCategory notify category =
  withCString category $ \p_category ->
  notify_notification_set_category notify p_category

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category :: Ptr Notification -> CString -> IO ()

setUrgency :: Ptr Notification -> Urgency -> IO ()
setUrgency notify urgency =
  notify_notification_set_urgency notify (getUrgency urgency)

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
