module Settings where

import Data.List

server = "lindbohm.freenode.net"
port   = 6667 :: Int
nick   = "hsbot"
name   = "hsbot"

-- Default channels to join
channels = intercalate ","
         [ "#clojure"
         , "#inimino"
         , "#jquery"
         , "#jquery-ot"
         , "#jquery-dev"
         , "#jquery-meeting"
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

waKey = "GAEW7L-JLAX59HR35"
