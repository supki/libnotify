{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

module Main where

import Control.Monad
import Data.IORef
import Data.Maybe
import Reactive.Banana
import System.Environment
import System.Exit
import System.Libnotify
import System.Posix
import qualified Data.Map as M
import qualified Network.MPD as MPD

data EventHandler = EventHandler
  { onSongChange :: (MPD.Song, MPD.State) -> IO ()
  , onStateChange :: (MPD.Song, MPD.State) -> IO ()
  }

deriving instance Typeable MPD.Song
deriving instance Typeable MPD.State

notifySend :: String -> String -> IO Notification
notifySend summary body = do
  notify <- newNotify summary
            (Just body)
            (Just "/usr/share/icons/gnome-colors-common/scalable/mimetypes/sound.svg")
  showNotify notify
  return notify

notifySong :: MPD.Song -> IO Notification
notifySong song = notifySend (foldr escape "" title) (foldr escape "" artist)
  where
    tag = MPD.sgTags song
    title = fromMaybe "No Title"  $ head <$> M.lookup MPD.Title tag
    artist = fromMaybe "No Artist" $ head <$> M.lookup MPD.Artist tag
    escape '<'  = ("&lt;" ++)
    escape '>'  = ("&gt;" ++)
    escape '\"' = ("&quot;" ++)
    escape '\'' = ("&#39;" ++)
    escape '&'  = ("&amp;" ++)
    escape x    = (x :)

eventLoop :: EventHandler -> IO (MPD.Response ())
eventLoop handler = MPD.withMPD $ do
  song  <- fromMaybe MPD.defaultSong <$> MPD.currentSong
  state <- MPD.stState <$> MPD.status
  loop song state
    where
      loop lastSong lastState = do
        liftIO $ sleep 1
        maybeSong <- MPD.currentSong
        state     <- MPD.stState <$> MPD.status
        case maybeSong of
          Just song -> do
            liftIO $ case () of
              _ | song /= lastSong   -> onSongChange handler (song, state)
                | state /= lastState -> onStateChange handler (song, state)
                | otherwise          -> return ()
            loop song state
          Nothing -> loop lastSong state

main = do
  getProgName >>= initNotify >>= flip unless exitFailure

  (addSongHandler, runSongHandler)   <- newAddHandler
  (addStateHandler, runStateHandler) <- newAddHandler

  network <- compile $ do
    lastNotify <- liftIO $ newIORef Nothing

    eSongChange  <- fromAddHandler $ addSongHandler
    eStateChange <- fromAddHandler $ addStateHandler

    let eStatePlay = filterE ((==) MPD.Playing . snd) eStateChange
        eStateStop = filterE ((/=) MPD.Playing . snd) eStateChange

    reactimate $ (<$ eSongChange `union` eStateStop) $ do
      maybeNotify <- readIORef lastNotify
      maybe (return True) closeNotify maybeNotify
      writeIORef lastNotify Nothing

    reactimate $ (<$> eSongChange `union` eStatePlay) $ \(song, _) -> do
      notifySong song >>= writeIORef lastNotify . Just

  actuate network

  eventLoop $ EventHandler
    { onSongChange  = runSongHandler
    , onStateChange = runStateHandler
    }
