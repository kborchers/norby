module Settings where

import           Data.List

server = "lindbohm.freenode.net"
port   = 6667 :: Int
nick   = "hsbot"
name   = "hsbot"

channels = intercalate "," [ "#clojure"
                           , "#inimino"
                           , "#jquery"
                           , "#jquery-ot"
                           , "#nlogax"
                           , "#runlevel6"
                           ]

-- Authorized bot wranglers
admins = [ "ajpiano"
         , "akahn"
         , "BBonifield"
         , "coldhead"
         , "gf3"
         , "matjas"
         , "miketaylr"
         , "nimbupani"
         , "nlogax"
         , "paul_irish"
         , "seutje"
         , "temp01"
         ]
