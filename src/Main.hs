{-# LANGUAGE OverloadedStrings #-}

import           Control.OldException
import           Control.Monad.Reader hiding (join)
import           Prelude hiding (catch)
import           System.IO

import           IRC
import           Settings
import           Types

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket (connect server port) disconnect loop
       where disconnect = hClose . socket
             loop st    = catch (runReaderT run st) (const $ return ())

-- Join some channels, and start processing commands
run :: Net ()
run = do
    write $ Message Nothing "NICK" [nick]
    write $ Message Nothing "USER" [nick, "0", "*", name]
    write $ Message Nothing "JOIN" [channels]
    asks socket >>= listen
