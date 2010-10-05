module Settings where

import           Data.List

server = "chat.us.freenode.net"
port   = 6667 :: Int
nick   = "ultror"
name   = "ultror"

channels = intercalate "," ["#clojure",
                            "#jquery",
                            "#jquery-ot",
                            "#runlevel6",
                            "#ultror"]

-- Authorized bot wranglers
admins = nicks ["ajpiano akahn BBonifield coldhead gf3 matjas miketaylr",
                "nimbupani nlogax paul_irish seutje temp01"]
         where nicks = words . unlines
