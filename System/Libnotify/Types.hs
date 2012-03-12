{-# LANGUAGE DeriveDataTypeable #-}
-- | System.Libnotify.Types module is a collection of types is used in other modules.
-- This is reexported with System.Libnotify module. Perhaps it'll never be needed to import explicitly.
{-# OPTIONS_HADDOCK prune #-}

module System.Libnotify.Types
  ( Timeout(..), Urgency(..), Category
  , Title, Body, Icon
  , Key
  , ServerInfo(..)
  , Hint(..)
  , NotifyError(..)
  ) where

import Control.Exception (Exception)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Data.Typeable (Typeable)
import Data.Word (Word8)

-- | Urgency can be used by the notification server to prioritize notifications.
-- Although urgency does not work with notify-osd.
data Urgency
       = Low -- ^ Low priority notification.
       | Normal -- ^ Default notification urgency.
       | Critical -- ^ Critical notification that requires immediate attention.

-- | Timeout in seconds after which notification is closed.
-- Although timeout does not work with notify-osd.
data Timeout
       = Default -- ^ Default server timeout.
       | Custom Int -- ^ User defined timeout (in milliseconds).
       | Infinite -- ^ Notification will not expire until user pays attention to it.

-- | Category can be used by the notification server to filter or display the data in a certain way.
type Category = String

-- | Type synonim for notification title.
type Title = String
-- | Type synonim for notification body.
type Body = String
-- | Type synonim for notification icon.
type Icon = String

-- | Server information.
data ServerInfo = ServerInfo
  { serverName  :: String
  , serverVendor :: String
  , serverVersion :: String
  , serverSpecVersion :: String
  } deriving Show

-- | Hint is some setting (server-dependent) which comes with notification.
data Hint = HintInt String Int32
          | HintDouble String Double
          | HintString String String
          | HintByte String Word8
          | HintArray String BS.ByteString


-- | Libnotify errors.
data NotifyError
  = NotifyInitHasFailed  -- ^ notify_init() has failed.
  | NewCalledBeforeInit  -- ^ 'new' has called before notify_init().
  deriving (Show, Typeable)

instance Exception NotifyError
