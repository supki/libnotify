{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

#include <libnotify/notify.h>

module System.Libnotify.Types
  ( Notification
  , Timeout, getTimeout, expiresDefault, expiresNever, expires
  , Urgency, getUrgency, notifyUrgencyLow, notifyUrgencyNormal, notifyUrgencyCritical
  , Category
  , Title, Body, Icon
  ) where

import Foreign.C

data Notification

newtype Timeout = Timeout {getTimeout :: CInt}
#{enum Timeout, Timeout,
  expiresDefault = NOTIFY_EXPIRES_DEFAULT,
  expiresNever   = NOTIFY_EXPIRES_NEVER
}

expires :: Int -> Timeout
expires = Timeout . fromIntegral

newtype Urgency = Urgency {getUrgency :: CInt}
#{enum Urgency, Urgency,
  notifyUrgencyLow      = NOTIFY_URGENCY_LOW,
  notifyUrgencyNormal   = NOTIFY_URGENCY_NORMAL,
  notifyUrgencyCritical = NOTIFY_URGENCY_CRITICAL
}

type Category = String

type Title = String
type Body = Maybe String
type Icon = Maybe String
