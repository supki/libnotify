{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Low level bindings to libnotify
--
-- See also <https://developer.gnome.org/libnotify/0.7/NotifyNotification.html>. Haddocks here
-- are mostly excerpts from there
module System.Libnotify.C.NotifyNotification
  ( NotifyNotification
  , notify_notification_new
  , notify_notification_update
  , notify_notification_show
  , notify_notification_set_app_name
  , Timeout(..)
  , notify_notification_set_timeout
  , notify_notification_set_category
  , Urgency(..)
  , notify_notification_set_urgency
  , notify_notification_set_icon_from_pixbuf
  , notify_notification_set_image_from_pixbuf
  , notify_notification_set_hint_int32
  , notify_notification_set_hint_uint32
  , notify_notification_set_hint_double
  , notify_notification_set_hint_string
  , notify_notification_set_hint_byte
  , notify_notification_set_hint_byte_array
  , notify_notification_clear_hints
  , notify_notification_add_action
  , notify_notification_clear_actions
  , notify_notification_close
  , notify_notification_get_closed_reason
  ) where

#include <libnotify/notify.h>

import Control.Exception (throwIO)
import Data.Data (Typeable, Data)
import GHC.Generics (Generic)
import Foreign
import Foreign.C
import Graphics.UI.Gtk.Gdk.Pixbuf (Pixbuf)
import System.Glib.GError (GError)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS

-- | An opaque notification token
newtype NotifyNotification = NotifyNotification (Ptr NotifyNotification)

-- | Create a new NotifyNotification
--
-- Only summary is required
notify_notification_new
  :: String -- ^ Summary
  -> String -- ^ Body
  -> String -- ^ Icon (icon name or file name)
  -> IO NotifyNotification
notify_notification_new summary body icon =
  withCString summary $ \p_summary ->
  withCString body $ \p_body ->
  withCString icon $ \p_icon ->
  notify_notification_new_c p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_new"
  notify_notification_new_c :: CString -> CString -> CString -> IO NotifyNotification

-- | Update the notification text and icon
notify_notification_update
  :: NotifyNotification
  -> String -- ^ Summary
  -> String -- ^ Body
  -> String -- ^ Icon (icon name or file name)
  -> IO Bool
notify_notification_update notify summary body icon =
  withCString summary $ \p_summary ->
  withCString body $ \p_body ->
  withCString icon $ \p_icon ->
  notify_notification_update_c notify p_summary p_body p_icon

foreign import ccall unsafe "libnotify/notify.h notify_notification_update"
  notify_notification_update_c :: NotifyNotification -> CString -> CString -> CString -> IO Bool

-- | Display the notification on the screen
notify_notification_show :: NotifyNotification -> IO Bool
notify_notification_show notify =
  alloca $ \pp_error -> do
    poke pp_error nullPtr
    result  <- notify_notification_show_c notify pp_error
    p_error <- peek pp_error
    if p_error == nullPtr
      then
        return result
      else do
        gerror <- peek p_error
        g_error_free p_error
        throwIO gerror

foreign import ccall unsafe "libnotify/notify.h notify_notification_show"
  notify_notification_show_c :: NotifyNotification -> Ptr (Ptr GError) -> IO Bool

-- | Set the application name for the notification
--
-- Used to override an application name for a specific notification.
-- See also 'notify_init' and 'notify_set_app_name'
notify_notification_set_app_name :: NotifyNotification -> String -> IO ()
notify_notification_set_app_name notify name =
  withCString name $ \p_name ->
  notify_notification_set_app_name_c notify p_name

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_app_name"
  notify_notification_set_app_name_c :: NotifyNotification -> CString -> IO ()

-- | Timeout in seconds after which notification is closed.
data Timeout =
    Default    -- ^ Default server timeout.
  | Custom Int -- ^ User defined timeout (in milliseconds).
  | Infinite   -- ^ Notification will never expire
    deriving (Show, Eq, Typeable, Data, Generic)

-- | Set the timeout of the notification
notify_notification_set_timeout :: NotifyNotification -> Timeout -> IO ()
notify_notification_set_timeout notify timeout =
  notify_notification_set_timeout_c notify $ case timeout of
    Default  -> #const NOTIFY_EXPIRES_DEFAULT
    Infinite -> #const NOTIFY_EXPIRES_NEVER
    Custom t -> fromIntegral t

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout_c :: NotifyNotification -> CInt -> IO ()

-- | Set the category of the notification
notify_notification_set_category :: NotifyNotification -> String -> IO ()
notify_notification_set_category notify category =
  withCString category $ \p_category ->
  notify_notification_set_category_c notify p_category

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category_c :: NotifyNotification -> CString -> IO ()

-- | The urgency level of the notification
data Urgency =
    Low      -- ^ Low urgency. Used for unimportant notifications
  | Normal   -- ^ Normal urgency. Used for most standard notifications
  | Critical -- ^ Critical urgency. Used for very important notifications
    deriving (Show, Eq, Ord, Typeable, Data, Generic)

-- | Set the urgency level of the notification
notify_notification_set_urgency :: NotifyNotification -> Urgency -> IO ()
notify_notification_set_urgency notify urgency =
  notify_notification_set_urgency_c notify $ case urgency of
    Low      -> #const NOTIFY_URGENCY_LOW
    Normal   -> #const NOTIFY_URGENCY_NORMAL
    Critical -> #const NOTIFY_URGENCY_CRITICAL

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_urgency"
  notify_notification_set_urgency_c :: NotifyNotification -> CInt -> IO ()

-- | Set the icon in the notification from the 'Pixbuf'
notify_notification_set_icon_from_pixbuf :: NotifyNotification -> Pixbuf -> IO ()
notify_notification_set_icon_from_pixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_icon_from_pixbuf_c notify p_pixbuf
{-# DEPRECATED notify_notification_set_icon_from_pixbuf
      "Use notify_notification_set_image_from_pixbuf instead"
  #-}

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_icon_from_pixbuf"
  notify_notification_set_icon_from_pixbuf_c :: NotifyNotification -> Ptr Pixbuf -> IO ()

-- | Set the icon in the notification from the 'Pixbuf'
notify_notification_set_image_from_pixbuf :: NotifyNotification -> Pixbuf -> IO ()
notify_notification_set_image_from_pixbuf notify pixbuf =
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_image_from_pixbuf_c notify p_pixbuf

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_image_from_pixbuf"
  notify_notification_set_image_from_pixbuf_c :: NotifyNotification -> Ptr Pixbuf -> IO ()

-- | Set a hint with a 32-bit integer value
notify_notification_set_hint_int32 :: NotifyNotification -> String -> Int32 -> IO ()
notify_notification_set_hint_int32 notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_int32_c notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_int32"
  notify_notification_set_hint_int32_c :: NotifyNotification -> CString -> CInt -> IO ()

-- | Set a hint with an unsigned 32-bit integer value
notify_notification_set_hint_uint32 :: NotifyNotification -> String -> Word32 -> IO ()
notify_notification_set_hint_uint32 notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_uint32_c notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_uint32"
  notify_notification_set_hint_uint32_c :: NotifyNotification -> CString -> CUInt -> IO ()

-- | Set a hint with a double value
notify_notification_set_hint_double :: NotifyNotification -> String -> Double -> IO ()
notify_notification_set_hint_double notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_double_c notify p_key (realToFrac value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_double"
  notify_notification_set_hint_double_c :: NotifyNotification -> CString -> CDouble -> IO ()

-- | Set a hint with a string value
notify_notification_set_hint_string :: NotifyNotification -> String -> String -> IO ()
notify_notification_set_hint_string notify key value =
  withCString key $ \p_key ->
  withCString value $ \p_value ->
  notify_notification_set_hint_string_c notify p_key p_value

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_string"
  notify_notification_set_hint_string_c :: NotifyNotification -> CString -> CString -> IO ()

-- | Set a hint with a byte value
notify_notification_set_hint_byte :: NotifyNotification -> String -> Word8 -> IO ()
notify_notification_set_hint_byte notify key value =
  withCString key $ \p_key ->
  notify_notification_set_hint_byte_c notify p_key (fromIntegral value)

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte"
  notify_notification_set_hint_byte_c :: NotifyNotification -> CString -> CUChar -> IO ()

-- | Set a hint with a byte array value
notify_notification_set_hint_byte_array :: NotifyNotification -> String -> BS.ByteString -> IO ()
notify_notification_set_hint_byte_array notify key value =
  withCString key $ \p_key ->
  withArrayLen (BS.foldr' step [] value) $ \len p_bs ->
  notify_notification_set_hint_byte_array_c notify p_key p_bs (fromIntegral len)
    where
      step x xs = fromIntegral x:xs

foreign import ccall unsafe "libnotify/notify.h notify_notification_set_hint_byte_array"
  notify_notification_set_hint_byte_array_c :: NotifyNotification -> CString -> Ptr CUChar -> CSize -> IO ()

-- | Clear all hints
notify_notification_clear_hints :: NotifyNotification -> IO ()
notify_notification_clear_hints = notify_notification_clear_hints_c

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_hints"
  notify_notification_clear_hints_c :: NotifyNotification -> IO ()

type NotifyActionCallback a = NotifyNotification -> CString -> Ptr a -> IO ()
type FreeFunc a = Ptr a -> IO ()

-- | Add an action to a notification. When the action is invoked, the specified callback
-- function will be called
--
-- For the callback to be *actually* invoked, some kind of magical glib @mainLoop@ thing
-- should be running
notify_notification_add_action
  :: NotifyNotification
  -> String
  -> String
  -> (NotifyNotification -> String -> IO ())
  -> IO ()
notify_notification_add_action notify action label callback =
  withCString action $ \p_action ->
  withCString label $ \p_label -> do
    p_callback <- wrapActionCallback $ \notify' p_action' _ -> do
      action' <- peekCString p_action'
      callback notify' action'
    notify_notification_add_action_c notify p_action p_label p_callback nullPtr nullFunPtr

foreign import ccall unsafe "wrapper"
  wrapActionCallback :: NotifyActionCallback a -> IO (FunPtr (NotifyActionCallback a))

foreign import ccall unsafe "libnotify/notify.h notify_notification_add_action"
  notify_notification_add_action_c
    :: NotifyNotification
    -> CString
    -> CString
    -> FunPtr (NotifyActionCallback a)
    -> Ptr a
    -> FunPtr (FreeFunc a)
    -> IO ()

-- | Clear all actions
notify_notification_clear_actions :: NotifyNotification -> IO ()
notify_notification_clear_actions = notify_notification_clear_actions_c

foreign import ccall unsafe "libnotify/notify.h notify_notification_clear_actions"
  notify_notification_clear_actions_c :: NotifyNotification -> IO ()

-- | Hide the notification from the screen
notify_notification_close :: NotifyNotification -> IO Bool
notify_notification_close notify =
  alloca $ \pp_error -> do
    poke pp_error nullPtr
    result  <- notify_notification_close_c notify pp_error
    p_error <- peek pp_error
    if p_error == nullPtr
      then
        return result
      else do
        gerror <- peek p_error
        g_error_free p_error
        throwIO gerror

foreign import ccall safe "libnotify/notify.h notify_notification_close"
  notify_notification_close_c :: NotifyNotification -> Ptr (Ptr GError) -> IO Bool

-- | Get the closed reason code for the notification
notify_notification_get_closed_reason :: NotifyNotification -> IO Int
notify_notification_get_closed_reason = notify_notification_get_closed_reason_c

foreign import ccall unsafe "libnotify/notify.h notify_notification_get_closed_reason"
  notify_notification_get_closed_reason_c :: NotifyNotification -> IO Int

foreign import ccall unsafe "glib-object.h g_error_free"
  g_error_free :: Ptr GError -> IO ()
