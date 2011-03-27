module TestSettings where

import Data.List

server = "lindbohm.freenode.net"
port   = 6667 :: Int
nick   = "hsbot2"
name   = "hsbot2"

channels = intercalate "," [ "#nlogax" ]

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
