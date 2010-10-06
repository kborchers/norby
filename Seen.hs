{-# Language OverloadedStrings #-}

module Seen where

import           Control.Monad.State
import qualified Data.Bson as B
import           Data.List hiding (sort, insert)
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           Data.UString (u)
import           Database.MongoDB
import           Text.Printf
import           Types

import qualified Utils as U

hostIP = "127.0.0.1"
dbName = "seen"

run = (runNet .) . flip (runConn . useDb dbName)
connectDb  = runNet . connect $ host hostIP

store :: Message -> IO String
store (Message (Just (NickName nick _ _)) command params) = do
      conn <- connectDb
      now  <- getCurrentTime
      let mess = last params
      let chan = head params
      let message = ["nick" =: nick,
                     "text" =: mess,
                     "what" =: command,
                     "chan" =: chan,
                     "date" =: now]
      either (const $ return "MongoDB is down!")
             (\con -> run con (insert "messages" message) >> return "Stored.")
             conn

store (Message _ _ _) = return "LOL!!"

-- What to do about this mess?
seen (Message (Just (NickName n _ _)) _ params) = do
     -- ".seen ultror  " -> "ultror"
     let nick = U.trim . unwords  . take 1 . drop 1 . words . last $ params
     conn <- connectDb
     case conn of
          Left  _   -> return "MongoDB is down!"
          Right con -> do
                Right res <- run con (findNick nick)
                either (const $ return "My tubes appear to be malfunctioning.")
                       (result nick) res

     where findNick n =
                    findOne (select ["nick" =: Regex
                            (mconcat [u"^", u (escape' n), "$"]) "i"] "messages")
                            { sort = ["_id" =: (-1 :: Int)] }
           
           result nick (Just val) = do
                  now <- getCurrentTime
                  let txt = B.at "text" val :: String
                  let cmd = B.at "what" val :: String
                  let chn = B.at "chan" val :: String
                  let whn = B.at "date" val :: UTCTime
                  return $ printf "%s: %s %s" n (formatSeen nick txt cmd chn)
                                                (timeAgo now whn)
           result nick Nothing = return $
                                 printf "%s: %s means nothing to me." n nick

seen (Message _ _ _) = return "nlogax fails at pattern matching."

escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"
escape' []     = []
escape' (c:cs) = escape c ++ escape' cs

formatSeen nick msg "PRIVMSG" chan
    | "\SOHACTION" `isPrefixOf` msg = printf "%s was all like *%s %s* in %s" nick nick
                                             (U.excerpt 60 "..." . init . drop 8 $ U.trim msg)
                                             chan
    | otherwise                     = printf "%s said \"%s\" in %s" nick
                                              (U.excerpt 60 "..." $ U.trim msg)
                                              chan

formatSeen n m "PART" c = printf "%s left %s with the message \"%s\"" n c m
formatSeen n _ "JOIN" c = printf "%s joined %s" n c
formatSeen n m "QUIT" _ = printf "%s quit with the message \"%s\"" n m
formatSeen n m "NICK" _ = printf "%s changed nick to %s" n m
formatSeen _ _ _      _ = "did something unspeakable" :: String

timeAgo now before = concatTime . relTime . round $ diffUTCTime now before

relTime t | t <  s     = ["now"]
          | t == s     = ["1 second"]
          | t <  m     = [show t ++ " seconds"]
          | t <  m * 2 = ["1 minute"]            ++ rest m
          | t <  h     = [first m ++ " minutes"] ++ rest m
          | t <  h * 2 = ["1 hour"]              ++ rest h
          | t <  d     = [first h ++ " hours"]   ++ rest h
          | t <  d * 2 = ["1 day"]               ++ rest d
          | t <  w     = [first d ++ " days"]    ++ rest d
          | t <  w * 2 = ["1 week"]              ++ rest w
          | t <  w * 8 = [first w ++ " weeks"]   ++ rest w
          | otherwise  = ["a long time"]
          where first  = show . div t
                rest v | mod t v == 0 = []
                       | otherwise    = take 1 . relTime $ mod t v
                s = 1
                m = s * 60
                h = m * 60
                d = h * 24
                w = d * 7

concatTime xss@(x:_) | x == "now"      = x
                     | 1 == length xss = printf "%s ago" $ concat xss
                     | otherwise       = printf "%s and %s ago." (intercalate ", " $ init xss)
                                                                 (last xss)

concatTime [] = []
