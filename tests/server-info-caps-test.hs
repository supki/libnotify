#!/usr/bin/env runhaskell

import Control.Monad (mapM_)

import System.Libnotify.Server
import System.Libnotify.Types

main :: IO ()
main = getServerInfo >>= \si ->
         putStrLn "Server information" >>
         putStrLn ("name: " ++ serverName si) >>
         putStrLn ("vendor: " ++ serverVendor si) >>
         putStrLn ("verion: " ++ serverVersion si) >>
         putStrLn ("specs version: " ++ serverSpecVersion si) >>
       getServerCaps >>= \sc ->
         putStrLn "Available capabilities:" >>
         mapM_ print sc
