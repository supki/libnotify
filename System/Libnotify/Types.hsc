{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
-- | System.Libnotify.Types module is a collection of types is used in other modules.
-- This is reexported with System.Libnotify module. Perhaps it'll never be needed to import explicitly.
{-# OPTIONS_HADDOCK prune #-}

#include <libnotify/notify.h>

module System.Libnotify.Types
  ( Timeout, getTimeout, expiresDefault, expiresNever, expires
  , Urgency, getUrgency, notifyUrgencyLow, notifyUrgencyNormal, notifyUrgencyCritical
  , Category
  , Title, Body, Icon
  , Key
  , ServerInfo(..)
  , NotifyError(..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Foreign (Ptr)
import Foreign.C

-- | Timeout in seconds after which notification is closed.
newtype Timeout = Timeout {getTimeout :: CInt}
#{enum Timeout, Timeout,
  expiresDefault = NOTIFY_EXPIRES_DEFAULT,
  expiresNever   = NOTIFY_EXPIRES_NEVER
}

-- | Sets custom 'Timeout'.
expires :: Int -> Timeout
expires = Timeout . fromIntegral

-- | Urgency can be used by the notification server to filter or display the data in a certain way.
newtype Urgency = Urgency {getUrgency :: CInt}
#{enum Urgency, Urgency,
  notifyUrgencyLow      = NOTIFY_URGENCY_LOW,
  notifyUrgencyNormal   = NOTIFY_URGENCY_NORMAL,
  notifyUrgencyCritical = NOTIFY_URGENCY_CRITICAL
}

-- | Category can be used by the notification server to filter or display the data in a certain way.
type Category = String

-- | Type synonim for notification title.
type Title = String
-- | Type synonim for notification body.
type Body = String
-- | Type synonim for notification icon.
type Icon = String

-- | Type synonym for 'Hint' key type.
type Key = String

-- | Server information.
data ServerInfo = ServerInfo
  { serverName  :: String
  , serverVendor :: String
  , serverVersion :: String
  , serverSpecVersion :: String
  } deriving Show

-- | Libnotify errors.
data NotifyError
  = NotifyInitHasFailed  -- ^ notify_init() has failed.
  | NewCalledBeforeInit  -- ^ 'new' has called before notify_init().
  deriving (Show, Typeable)

instance Exception NotifyError
