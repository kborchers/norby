{-# LANGUAGE OverloadedStrings #-}

import           Control.OldException
import           Control.Monad.Reader hiding (join)
import           Data.List
import           Prelude hiding (catch)
import           System.IO

import           IRC hiding (message)
import           Types

--server = "wineasy.se.quakenet.org"
server = "chat.us.freenode.net"
port   = 6667
nickn  = "ultror"
name   = "ultror"

channels = intercalate "," ["#clojure",
                            "#jquery",
                            "#jquery-ot",
                            "#runlevel6",
                            "#ultror"]

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket (connect server port) disconnect loop
       where disconnect = hClose . socket
             loop st    = catch (runReaderT run st) (const $ return ())

-- Join some channels, and start processing commands
run :: Net ()
run = do
    write $ Message Nothing "USER" [nickn, "0", "*", name]
    write $ Message Nothing "NICK" [nickn]
    write $ Message Nothing "JOIN" [channels]
    asks socket >>= listen

-- Authorized bot wranglers
admins = nicks ["ajpiano akahn BBonifield coldhead gf3 matjas miketaylr",
                "nimbupani nlogax paul_irish seutje temp01"]
         where nicks = words . unlines
