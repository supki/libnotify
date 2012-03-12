{-# LANGUAGE ForeignFunctionInterface #-}
-- | System.Libnotify.Internal module is low level c-bindings to Libnotify.
-- No API stability is guaranteed.
{-# OPTIONS_HADDOCK hide #-}

#include <libnotify/notify.h>

module System.Libnotify.Internal
  ( Notification
  , initNotify, uninitNotify, isInitted
  , newNotify, updateNotify, showNotify
  , setTimeout, setCategory, setUrgency
  , setIconFromPixbuf, setImageFromPixbuf
  , setHintInt32, setHintDouble, setHintString, setHintByte, setHintByteArray, clearHints
  , addAction, clearActions, closeNotify
  ) where

import Control.Exception (throw)
import Foreign
import Foreign.C
import Graphics.UI.Gtk.Gdk.Pixbuf
import System.Glib.GError (GError)
import Unsafe.Coerce
import qualified Data.ByteString as BS

import System.Libnotify.Types

-- | Notification session pointer
newtype Notification = Notification (Ptr Notification)

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

type ActionCallback a = Notification -> CString -> Ptr a -> IO ()
type FreeFunc a = Ptr a -> IO ()

newNotify :: String -> Maybe String -> Maybe String -> IO Notification
newNotify summary body icon =
  withCString summary        $ \p_summary ->
  maybeWith withCString body $ \p_body ->
  maybeWith withCString icon $ \p_icon ->
  notify_notification_new p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_new"
  notify_notification_new :: CString
                          -> CString
                          -> CString
                          -> IO Notification

updateNotify :: Notification -> String -> Maybe String -> Maybe String -> IO Bool
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

showNotify :: Notification -> IO Bool
showNotify notify =
  alloca $ \pp_error -> do
  poke pp_error nullPtr
  result <- notify_notification_show notify pp_error
  p_error <- peek pp_error
  if p_error == nullPtr
    then return result
    else do gerror <- peek p_error
            g_error_free p_error
            throw gerror

foreign import ccall unsafe "libnotify/notify.h notify_notification_show"
  notify_notification_show :: Notification -> Ptr (Ptr GError) -> IO Bool

foreign import ccall unsafe "glib-object.h g_error_free"
  g_error_free :: Ptr GError -> IO ()

setTimeout :: Timeout -> Notification -> IO ()
setTimeout timeout notify =
  notify_notification_set_timeout notify (getTimeout timeout)
  where getTimeout :: Timeout -> CInt
        getTimeout Default    = #const NOTIFY_EXPIRES_DEFAULT
        getTimeout Infinite   = #const NOTIFY_EXPIRES_NEVER
        getTimeout (Custom t) = fromIntegral t

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout :: Notification -> CInt -> IO ()

setCategory :: Category -> Notification -> IO ()
setCategory category notify =
  withCString category $ \p_category ->
  notify_notification_set_category notify p_category

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category :: Notification -> CString -> IO ()

setUrgency :: Urgency -> Notification -> IO ()
setUrgency urgency notify =
  notify_notification_set_urgency notify (getUrgency urgency)
  where getUrgency :: Urgency -> CInt
        getUrgency Low      = #const NOTIFY_URGENCY_LOW
        getUrgency Normal   = #const NOTIFY_URGENCY_NORMAL
        getUrgency Critical = #const NOTIFY_URGENCY_CRITICAL

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_urgency"
  notify_notification_set_urgency :: Notification -> CInt -> IO ()

setIconFromPixbuf :: Pixbuf -> Notification -> IO ()
setIconFromPixbuf pixbuf notify =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_icon_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_icon_from_pixbuf"
  notify_notification_set_icon_from_pixbuf :: Notification
                                           -> Ptr Pixbuf
                                           -> IO ()

setImageFromPixbuf :: Pixbuf -> Notification -> IO ()
setImageFromPixbuf pixbuf notify =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_image_from_pixbuf notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_image_from_pixbuf"
  notify_notification_set_image_from_pixbuf :: Notification
                                            -> Ptr Pixbuf
                                            -> IO ()

setHintInt32 :: Notification -> String -> Int32 -> IO ()
setHintInt32 notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_int32 notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_int32"
  notify_notification_set_hint_int32 :: Notification
                                     -> CString
                                     -> CInt
                                     -> IO ()

setHintDouble :: Notification -> String -> Double -> IO ()
setHintDouble notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_double notify p_key (realToFrac value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_double"
  notify_notification_set_hint_double :: Notification
                                      -> CString
                                      -> CDouble
                                      -> IO ()

setHintString :: Notification -> String -> String -> IO ()
setHintString notify key value =
  withCString key   $ \p_key ->
  withCString value $ \p_value ->
  notify_notification_set_hint_string notify p_key p_value

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_string"
  notify_notification_set_hint_string :: Notification
                                      -> CString
                                      -> CString
                                      -> IO ()

setHintByte :: Notification -> String -> Word8 -> IO ()
setHintByte notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_byte notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte"
  notify_notification_set_hint_byte :: Notification
                                    -> CString
                                    -> CUChar
                                    -> IO ()

setHintByteArray :: Notification -> String -> BS.ByteString -> IO ()
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

clearHints :: Notification -> IO ()
clearHints = notify_notification_clear_hints

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_hints"
  notify_notification_clear_hints :: Notification -> IO ()

addAction
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

clearActions :: Notification -> IO ()
clearActions = notify_notification_clear_actions

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_actions"
  notify_notification_clear_actions :: Notification -> IO ()

closeNotify :: Notification -> IO Bool
closeNotify notify =
  alloca $ \pp_error -> do
  poke pp_error nullPtr
  result <- notify_notification_close notify pp_error
  p_error <- peek pp_error
  if p_error == nullPtr
    then return result
    else do gerror <- peek p_error
            g_error_free p_error
            throw gerror

foreign import ccall unsafe "libnotify/notify.h notify_notification_close"
  notify_notification_close :: Notification -> Ptr (Ptr GError) -> IO Bool
