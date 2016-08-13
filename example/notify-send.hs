{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Data.Bool (bool)
import           Data.Int (Int32)
import           Data.Maybe (catMaybes)
import           Data.Word (Word8)
import qualified Libnotify as L
import           Options.Applicative
import           Prelude hiding (mod)
import           Text.Printf (printf)
import           Text.Read (readEither)


main :: IO ()
main = do
  Options {summary, body, mod} <- parseOpts
  L.display_ (mod <> L.summary summary <> foldMap L.body body)

data Options = Options
  { summary :: String
  , body    :: Maybe String
  , mod     :: L.Mod L.Notification
  }

parseOpts :: IO Options
parseOpts =
  customExecParser (prefs showHelpOnError) (info (helper <*> parser) desc)
 where
  desc =
    fullDesc <> progDesc "Create a notification." <> header "notify-send - an example of the libnotify library"
  parser = do
    summary <- argument str (metavar "SUMMARY")
    body <- optional (argument str (metavar "BODY"))
    mod <- parserMod
    pure Options {..}
  parserMod =
    fmap (mconcat . catMaybes) . traverse optional $
      [ option urgency  (short 'u' <> long "urgency"  <> metavar "LEVEL")
      , option timeout  (short 't' <> long "timeout"  <> metavar "TIME")
      , option appName  (short 'a' <> long "app-name" <> metavar "APP_NAME")
      , option icon     (short 'i' <> long "icon"     <> metavar "ICON")
      , option category (short 'c' <> long "category" <> metavar "TIME")
      , option hint     (short 'h' <> long "hint"     <> metavar "TYPE:NAME:VALUE")
      ]
  urgency =
    eitherReader $ \case
      "low" ->
        pure (L.urgency L.Low)
      "normal" ->
        pure (L.urgency L.Normal)
      "critical" ->
        pure (L.urgency L.Critical)
      urg ->
        Left (printf "Unknown urgency `%s'. Known urgencies: `low', `normal', and `critical'." urg)
  timeout = do
    time <- auto
    pure (L.timeout (bool (L.Custom time) L.Infinite (time == 0)))
  appName = do
    name <- str
    pure (L.appName name)
  icon = do
    name <- str
    pure (L.icon name)
  category = do
    cat <- str
    pure (L.category cat)
  hint :: ReadM (L.Mod L.Notification)
  hint =
    eitherReader $ \typeNameVal ->
      case break (== ':') typeNameVal of
        (type_, ':' : nameVal) ->
          case break (== ':') nameVal of
            (name, ':' : val) ->
              case type_ of
                "int" -> do
                  int <- readEither val
                  pure (L.hint name (int :: Int32))
                "double" -> do
                  double <- readEither val
                  pure (L.hint name (double :: Double))
                "string" ->
                  pure (L.hint name val)
                "byte" -> do
                  byte <- readEither val
                  pure (L.hint name (byte :: Word8))
                _ ->
                  Left (printf "Invalid hint: `%s`" typeNameVal)
            _ ->
              Left (printf "Invalid hint: `%s`" typeNameVal)
        _ ->
          Left (printf "Invalid hint: `%s`" typeNameVal)
