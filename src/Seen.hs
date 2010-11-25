{-# Language OverloadedStrings #-}

module Seen where

import qualified Data.Bson as B
import           Data.List hiding (sort, insert)
import           Data.Monoid
import           Data.Time
import           Data.UString (u)
import           Database.MongoDB
import qualified Settings as S
import           Text.Printf
import           Types

import qualified Utils as U

hostIP = "127.0.0.1"
dbName = "seen"
collection = "messages"

run c d    = runNet $ runConn (useDb dbName d) c
connectDb  = runNet . connect $ host hostIP

store (Message (Just (NickName nick _ _)) cmd params) = do
      conn <- connectDb
      now  <- getCurrentTime
      let mess = last params
      let chan = head params
      let message = ["nick" =: nick, "text" =: mess, "what" =: cmd,
                     "chan" =: chan, "date" =: now]
      either (const $ return "MongoDB is down!")
             ((>> return "Stored.") . flip run (insert collection message))
             conn

store (Message _ _ _) = return "LOL!!" :: IO String

seen (Message (Just (NickName n _ _)) _ params)
    | n    == nick   = return $ printf "%s: That's you, I see you in %s right now." n chan
    | nick == S.nick = return $ printf "%s: That's me, I am here in %s." n chan
    | otherwise      = do
        conn <- connectDb
        either (const $ return "MongoDB is down!")
               (\c -> run c (findNick nick) >>= \a -> case a of
                      Right (Right v) -> result v
                      _               -> return "Kasplode") conn
     
     where findNick n =
                    findOne (select ["nick" =: Regex
                            (mconcat [u"^", u (escape' n), "$"]) "i"] collection)
                            { sort = ["_id" =: (-1 :: Int)] }
           
           result (Just val) = do
                  now <- getCurrentTime
                  let txt = B.at "text" val :: String
                  let cmd = B.at "what" val :: String
                  let chn = B.at "chan" val :: String
                  let whn = B.at "date" val :: UTCTime
                  return $ printf "%s: %s %s" n (formatSeen nick txt cmd chn)
                                                (timeAgo now whn)
           result Nothing = return $
                            printf "%s: %s means nothing to me." n nick
           timeAgo = ((concatTime . relTime . round) .) . diffUTCTime
           nick = U.trim . concat . take 1 . drop 1 . words . last $ params
           chan = concat $ take 1 params

seen (Message _ _ _) = return "nlogax fails at pattern matching."

escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"
escape' []     = []
escape' (c:cs) = escape c ++ escape' cs

formatSeen nick msg "PRIVMSG" chan
    | "\SOHACTION" `isPrefixOf` msg = printf "%s was all like *%s %s* in %s" nick nick
                                             (U.excerpt 100 "..." . init . drop 8 $ U.trim msg)
                                             chan
    | otherwise = printf "%s said \"%s\" in %s" nick
                         (U.excerpt 100 "..." $ U.trim msg) chan

formatSeen n m cmd c = case cmd of
    "PART" -> printf "%s left %s"   n c
    "JOIN" -> printf "%s joined %s" n c
    "QUIT" -> printf "%s quit with the message \"%s\"" n m'
    "NICK" -> printf "%s changed nick to %s" n m
    _      -> "did something unspeakable" :: String
    where m' = U.excerpt' $ U.trim m

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
          | t <  w * 4 = [first w ++ " weeks"]   ++ rest w
          | otherwise  = ["a long time"]
          where first  = show . div t
                rest v | mod t v == 0 = []
                       | otherwise    = relTime $ mod t v
                s = 1; m = s * 60; h = m * 60; d = h * 24; w = d * 7

concatTime xss@(x:_) | null xss        = xss
                     | x == "now"      = x
                     | 1 == length xss = printf "%s ago." $ concat xss
                     | otherwise       = printf "%s and %s ago."
                                                (intercalate ", " $ init xss)
                                                                  $ last xss
