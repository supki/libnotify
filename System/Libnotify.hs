{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module System.Libnotify
  ( Title, Body, Icon
  , withNotifications
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

import qualified System.Libnotify.Internal as N
import System.Libnotify.Types

withNotifications :: Maybe String -> IO a -> IO ()
withNotifications a x = do initted <- N.initNotify appName
                           if initted
                             then x >> N.uninitNotify
                             else error "withNotifications: init has failed."
  where appName = case a of
                    Just name -> name
                    Nothing   -> " "

oneShot :: Hint a => Title -> Body -> Icon -> [a] -> IO ()
oneShot t b i hs = withNotifications Nothing $
                   new t b i $ do mapM_ addHint hs
                                  render

new :: Title -> Body -> Icon -> ReaderT (Ptr Notification) IO b -> IO b
new t b i f = do n <- N.newNotify t b i
                 runReaderT f n

update :: (MonadIO m, MonadReader (Ptr Notification) m) => Title -> Body -> Icon -> m Bool
update t b i = do n <- ask
                  liftIO $ N.updateNotify n t b i

render :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
render = do n <- ask
            liftIO $ N.showNotify n

close :: (MonadIO m, MonadReader (Ptr Notification) m) => m Bool
close = do n <- ask
           liftIO $ N.closeNotify n

timeout :: (MonadIO m, MonadReader (Ptr Notification) m) => Timeout -> m ()
timeout t = do n <- ask
               liftIO $ N.setTimeout n t

category :: (MonadIO m, MonadReader (Ptr Notification) m) => Category -> m ()
category c = do n <- ask
                liftIO $ N.setCategory n c

urgency :: (MonadIO m, MonadReader (Ptr Notification) m) => Urgency -> m ()
urgency u = do n <- ask
               liftIO $ N.setUrgency n u

data GeneralHint = HintInt String Int32 | HintDouble String Double | HintString String String | HintByte String Word8 | HintArray String BS.ByteString
type Key = String

class Hint a where
  addHint :: a -> (MonadIO m, MonadReader (Ptr Notification) m) => m ()
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
  generalize (k,v) = HintInt k v

instance Hint (Key,Double) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintDouble n k v
  generalize (k,v) = HintDouble k v

instance Hint (Key,String) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintString n k v
  generalize (k,v) = HintString k v

instance Hint (Key,Word8) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintByte n k v
  generalize (k,v) = HintByte k v

instance Hint (Key,BS.ByteString) where
  addHint (k,v) = do n <- ask
                     liftIO $ N.setHintByteArray n k v
  generalize (k,v) = HintArray k v

clearHints :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
clearHints = do n <- ask
                liftIO $ N.clearHints n

addAction :: (MonadIO m, MonadReader (Ptr Notification) m) => String -> String -> (Ptr Notification -> String -> IO ()) -> m ()
addAction a l c = do n <- ask
                     liftIO $ N.addAction n a l c

clearActions :: (MonadIO m, MonadReader (Ptr Notification) m) => m ()
clearActions = do n <- ask
                  liftIO $ N.clearActions n
