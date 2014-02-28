{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Low level bindings to libnotify
--
-- See also <https://developer.gnome.org/libnotify/0.7/NotifyNotification.html>. Haddocks here
-- are mostly excerpts from there
module Libnotify.C.NotifyNotification
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
import System.Glib.GObject (GObjectClass(..), GObject(..), unGObject, wrapNewGObject, objectUnref)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS

-- | An opaque notification token
newtype NotifyNotification = NotifyNotification (ForeignPtr NotifyNotification)
  deriving (Show, Eq)

instance GObjectClass NotifyNotification where
  toGObject (NotifyNotification p) = GObject (castForeignPtr p)
  unsafeCastGObject = NotifyNotification . castForeignPtr . unGObject

-- | Create a new 'NotifyNotification'
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
  wrapNewGObject (NotifyNotification, objectUnref) $
    notify_notification_new_c p_summary p_body p_icon


-- | Update the notification text and icon
notify_notification_update
  :: NotifyNotification
  -> String -- ^ Summary
  -> String -- ^ Body
  -> String -- ^ Icon (icon name or file name)
  -> IO Bool
notify_notification_update (NotifyNotification notify) summary body icon =
  withCString summary $ \p_summary ->
  withCString body $ \p_body ->
  withCString icon $ \p_icon ->
  withForeignPtr notify $ \p_notify ->
  notify_notification_update_c p_notify p_summary p_body p_icon

-- | Display the notification on the screen
notify_notification_show :: NotifyNotification -> IO Bool
notify_notification_show (NotifyNotification notify) =
  withForeignPtr notify $ \p_notify ->
  alloca $ \pp_error -> do
    poke pp_error nullPtr
    result  <- notify_notification_show_c p_notify pp_error
    p_error <- peek pp_error
    if p_error == nullPtr
      then
        return result
      else do
        gerror <- peek p_error
        g_error_free p_error
        throwIO gerror

-- | Set the application name for the notification
--
-- Used to override an application name for a specific notification.
-- See also 'notify_init' and 'notify_set_app_name'
notify_notification_set_app_name :: NotifyNotification -> String -> IO ()
notify_notification_set_app_name (NotifyNotification notify) name =
  withForeignPtr notify $ \p_notify ->
  withCString name $ \p_name ->
  notify_notification_set_app_name_c p_notify p_name

-- | Timeout after which notification is closed
data Timeout =
    Default    -- ^ Default server timeout
  | Custom Int -- ^ User defined timeout (in milliseconds)
  | Infinite   -- ^ Notification will never expire
    deriving (Show, Eq, Typeable, Data, Generic)

-- | Set the timeout of the notification
notify_notification_set_timeout :: NotifyNotification -> Timeout -> IO ()
notify_notification_set_timeout (NotifyNotification notify) timeout =
  withForeignPtr notify $ \p_notify ->
  notify_notification_set_timeout_c p_notify $ case timeout of
    Default  -> #const NOTIFY_EXPIRES_DEFAULT
    Infinite -> #const NOTIFY_EXPIRES_NEVER
    Custom t -> fromIntegral t

-- | Set the category of the notification
notify_notification_set_category :: NotifyNotification -> String -> IO ()
notify_notification_set_category (NotifyNotification notify) category =
  withForeignPtr notify $ \p_notify ->
  withCString category $ \p_category ->
  notify_notification_set_category_c p_notify p_category

-- | The urgency level of the notification
data Urgency =
    Low      -- ^ Low urgency. Used for unimportant notifications
  | Normal   -- ^ Normal urgency. Used for most standard notifications
  | Critical -- ^ Critical urgency. Used for very important notifications
    deriving (Show, Eq, Ord, Typeable, Data, Generic)

-- | Set the urgency level of the notification
notify_notification_set_urgency :: NotifyNotification -> Urgency -> IO ()
notify_notification_set_urgency (NotifyNotification notify) urgency =
  withForeignPtr notify $ \p_notify ->
  notify_notification_set_urgency_c p_notify $ case urgency of
    Low      -> #const NOTIFY_URGENCY_LOW
    Normal   -> #const NOTIFY_URGENCY_NORMAL
    Critical -> #const NOTIFY_URGENCY_CRITICAL

-- | Set the icon in the notification from the 'Pixbuf'
notify_notification_set_icon_from_pixbuf :: NotifyNotification -> Pixbuf -> IO ()
notify_notification_set_icon_from_pixbuf (NotifyNotification notify) pixbuf =
  withForeignPtr notify $ \p_notify ->
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_icon_from_pixbuf_c p_notify p_pixbuf
{-# DEPRECATED notify_notification_set_icon_from_pixbuf
      "Use notify_notification_set_image_from_pixbuf instead" #-}

-- | Set the icon in the notification from the 'Pixbuf'
notify_notification_set_image_from_pixbuf :: NotifyNotification -> Pixbuf -> IO ()
notify_notification_set_image_from_pixbuf (NotifyNotification notify) pixbuf =
  withForeignPtr notify $ \p_notify ->
  withForeignPtr (unsafeCoerce pixbuf) $ \p_pixbuf ->
  notify_notification_set_image_from_pixbuf_c p_notify p_pixbuf

-- | Set a hint with a 32-bit integer value
notify_notification_set_hint_int32 :: NotifyNotification -> String -> Int32 -> IO ()
notify_notification_set_hint_int32 (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  notify_notification_set_hint_int32_c p_notify p_key (fromIntegral value)

-- | Set a hint with an unsigned 32-bit integer value
notify_notification_set_hint_uint32 :: NotifyNotification -> String -> Word32 -> IO ()
notify_notification_set_hint_uint32 (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  notify_notification_set_hint_uint32_c p_notify p_key (fromIntegral value)

-- | Set a hint with a double value
notify_notification_set_hint_double :: NotifyNotification -> String -> Double -> IO ()
notify_notification_set_hint_double (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  notify_notification_set_hint_double_c p_notify p_key (realToFrac value)

-- | Set a hint with a string value
notify_notification_set_hint_string :: NotifyNotification -> String -> String -> IO ()
notify_notification_set_hint_string (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  withCString value $ \p_value ->
  notify_notification_set_hint_string_c p_notify p_key p_value

-- | Set a hint with a byte value
notify_notification_set_hint_byte :: NotifyNotification -> String -> Word8 -> IO ()
notify_notification_set_hint_byte (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  notify_notification_set_hint_byte_c p_notify p_key (fromIntegral value)

-- | Set a hint with a byte array value
notify_notification_set_hint_byte_array :: NotifyNotification -> String -> BS.ByteString -> IO ()
notify_notification_set_hint_byte_array (NotifyNotification notify) key value =
  withForeignPtr notify $ \p_notify ->
  withCString key $ \p_key ->
  withArrayLen (BS.foldr' step [] value) $ \len p_bs ->
  notify_notification_set_hint_byte_array_c p_notify p_key p_bs (fromIntegral len)
    where
      step x xs = fromIntegral x:xs

-- | Clear all hints
notify_notification_clear_hints :: NotifyNotification -> IO ()
notify_notification_clear_hints (NotifyNotification notify) =
  withForeignPtr notify notify_notification_clear_hints_c

type NotifyActionCallback a = Ptr NotifyNotification -> CString -> Ptr a -> IO ()

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
notify_notification_add_action (NotifyNotification notify) action label callback =
  withForeignPtr notify $ \p_notify ->
  withCString action $ \p_action ->
  withCString label $ \p_label -> do
    p_callback <- wrapActionCallback $ \p_notify' p_action' _ -> do
      action' <- peekCString p_action'
      fp_notify' <- newForeignPtr_ p_notify'
      callback (NotifyNotification fp_notify') action'
    notify_notification_add_action_c p_notify p_action p_label p_callback nullPtr nullFunPtr

-- | Clear all actions
notify_notification_clear_actions :: NotifyNotification -> IO ()
notify_notification_clear_actions (NotifyNotification notify) =
  withForeignPtr notify notify_notification_clear_actions_c

-- | Hide the notification from the screen
notify_notification_close :: NotifyNotification -> IO Bool
notify_notification_close (NotifyNotification notify) =
  withForeignPtr notify $ \p_notify ->
  alloca $ \pp_error -> do
    poke pp_error nullPtr
    result  <- notify_notification_close_c p_notify pp_error
    p_error <- peek pp_error
    if p_error == nullPtr
      then
        return result
      else do
        gerror <- peek p_error
        g_error_free p_error
        throwIO gerror

-- | Get the closed reason code for the notification
notify_notification_get_closed_reason :: NotifyNotification -> IO Int
notify_notification_get_closed_reason (NotifyNotification notify) =
  withForeignPtr notify notify_notification_get_closed_reason_c

foreign import ccall safe "libnotify/notify.h notify_notification_new"
  notify_notification_new_c :: CString -> CString -> CString -> IO (Ptr NotifyNotification)

foreign import ccall safe "libnotify/notify.h notify_notification_update"
  notify_notification_update_c :: Ptr NotifyNotification -> CString -> CString -> CString -> IO Bool

foreign import ccall safe "libnotify/notify.h notify_notification_show"
  notify_notification_show_c :: Ptr NotifyNotification -> Ptr (Ptr GError) -> IO Bool

foreign import ccall safe "libnotify/notify.h notify_notification_set_app_name"
  notify_notification_set_app_name_c :: Ptr NotifyNotification -> CString -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_timeout"
  notify_notification_set_timeout_c :: Ptr NotifyNotification -> CInt -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_category"
  notify_notification_set_category_c :: Ptr NotifyNotification -> CString -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_urgency"
  notify_notification_set_urgency_c :: Ptr NotifyNotification -> CInt -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_icon_from_pixbuf"
  notify_notification_set_icon_from_pixbuf_c :: Ptr NotifyNotification -> Ptr Pixbuf -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_image_from_pixbuf"
  notify_notification_set_image_from_pixbuf_c :: Ptr NotifyNotification -> Ptr Pixbuf -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_int32"
  notify_notification_set_hint_int32_c :: Ptr NotifyNotification -> CString -> CInt -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_uint32"
  notify_notification_set_hint_uint32_c :: Ptr NotifyNotification -> CString -> CUInt -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_double"
  notify_notification_set_hint_double_c :: Ptr NotifyNotification -> CString -> CDouble -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_string"
  notify_notification_set_hint_string_c :: Ptr NotifyNotification -> CString -> CString -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_byte"
  notify_notification_set_hint_byte_c :: Ptr NotifyNotification -> CString -> CUChar -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_set_hint_byte_array"
  notify_notification_set_hint_byte_array_c :: Ptr NotifyNotification -> CString -> Ptr CUChar -> CSize -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_clear_hints"
  notify_notification_clear_hints_c :: Ptr NotifyNotification -> IO ()

foreign import ccall safe "wrapper"
  wrapActionCallback :: NotifyActionCallback a -> IO (FunPtr (NotifyActionCallback a))

foreign import ccall safe "libnotify/notify.h notify_notification_add_action"
  notify_notification_add_action_c
    :: Ptr NotifyNotification
    -> CString
    -> CString
    -> FunPtr (NotifyActionCallback a)
    -> Ptr a
    -> FunPtr (Ptr a -> IO ())
    -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_clear_actions"
  notify_notification_clear_actions_c :: Ptr NotifyNotification -> IO ()

foreign import ccall safe "libnotify/notify.h notify_notification_close"
  notify_notification_close_c :: Ptr NotifyNotification -> Ptr (Ptr GError) -> IO Bool

foreign import ccall safe "libnotify/notify.h notify_notification_get_closed_reason"
  notify_notification_get_closed_reason_c :: Ptr NotifyNotification -> IO Int

foreign import ccall safe "glib-object.h g_error_free"
  g_error_free :: Ptr GError -> IO ()
