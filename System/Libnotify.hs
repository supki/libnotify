{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module System.Libnotify
  ( withNotifications
  , new, update, render, close
  , timeout, category, urgency
  , addHint, generalize, clearHints
  , addAction, clearActions
  , oneShot) where

import Control.Monad.Reader
import Data.Word (Word8)
import Data.Int (Int32)
import qualified Data.ByteString as BS
import Foreign (Ptr)
import Prelude hiding (show)

import qualified System.Libnotify.Internal as N

withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = do initted <- N.initNotify appName
                           if initted
                             then x >> N.uninitNotify
                             else error "withNotifications: init has failed."
  where appName = case a of
                    Just name -> name
                    Nothing   -> " "

type Title = String
type Body = String
type Icon = String

oneShot :: Hint a => Title -> Maybe Body -> Maybe Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing $
                   new t b i $ do mapM_ addHint hs
                                  render

new :: String -> Maybe String -> Maybe String -> ReaderT (Ptr N.Notification) IO b -> IO b
new t b i f = do n <- N.newNotify t b i
                 runReaderT f n

update :: (MonadIO m, MonadReader (Ptr N.Notification) m) => String -> Maybe String -> Maybe String -> m Bool
update t b i = do n <- ask
                  liftIO $ N.updateNotify n t b i

render :: (MonadIO m, MonadReader (Ptr N.Notification) m) => m Bool
render = do n <- ask
            liftIO $ N.showNotify n

close :: (MonadIO m, MonadReader (Ptr N.Notification) m) => m Bool
close = do n <- ask
           liftIO $ N.closeNotify n

timeout :: (MonadIO m, MonadReader (Ptr N.Notification) m) => N.NotificationTimeout -> m ()
timeout t = do n <- ask
               liftIO $ N.setTimeout n t

category :: (MonadIO m, MonadReader (Ptr N.Notification) m) => String -> m ()
category c = do n <- ask
                liftIO $ N.setCategory n c

urgency :: (MonadIO m, MonadReader (Ptr N.Notification) m) => N.Urgency -> m ()
urgency u = do n <- ask
               liftIO $ N.setUrgency n u

data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString
type Key = String

class Hint a where
  addHint :: a -> (MonadIO m, MonadReader (Ptr N.Notification) m) => m ()
  generalize :: a -> GeneralHint

instance Hint GeneralHint where
  addHint (HintInt k v) = addHint (k,v)
  addHint (HintDouble k v) = addHint (k,v)
  addHint (HintString k v) = addHint (k,v)
  addHint (HintByte k v) = addHint (k,v)
  addHint (HintArray k v) = addHint (k,v)
  generalize = id

instance Hint (Key,Int32) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintInt32 n k v
  generalize (k, v) = HintInt k v

instance Hint (Key,Double) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintDouble n k v
  generalize (k, v) = HintDouble k v

instance Hint (Key,String) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintString n k v
  generalize (k, v) = HintString k v

instance Hint (Key,Word8) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintByte n k v
  generalize (k, v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintByteArray n k v
  generalize (k, v) = HintArray k v

clearHints :: (MonadIO m, MonadReader (Ptr N.Notification) m) => m ()
clearHints = do n <- ask
                liftIO $ N.clearHints n

addAction :: (MonadIO m, MonadReader (Ptr N.Notification) m) => String -> String -> (Ptr N.Notification -> String -> IO ()) -> m ()
addAction a l c = do n <- ask
                     liftIO $ N.addAction n a l c

clearActions :: (MonadIO m, MonadReader (Ptr N.Notification) m) => m ()
clearActions = do n <- ask
                  liftIO $ N.clearActions n
