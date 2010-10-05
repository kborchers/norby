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
import           System.Random
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
seen :: Message -> IO String
seen (Message (Just (NickName n _ _)) _ params) = do
     -- ".seen ultror  " -> "ultror"
     let nick = U.trim . unwords  . take 1 . drop 1 . words . last $ params
     conn <- connectDb
     case conn of
          Left  _   -> return "MongoDB is down!"
          Right con -> do
                Right res <- run con (findNick nick)
                either (const $ return "Everyone died!")
                       (internet n nick) res
     where findNick n = findOne (select
                                ["nick" =: Regex
                                          (mconcat [u"^", u (escape' n), "$"])
                                          "i"]
                                "messages") { sort = ["date" =: (-1 :: Int)] }

seen (Message _ _ _) = return "nlogax fails at pattern matching."

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
         where regexChars = "\\+()^$.{}]|"
escape' []      = []
escape' (c:cs) = escape c ++ escape' cs

internet n nick val = do
  now  <- getCurrentTime
  let  txt = ((B.lookup "text" $ fromMaybe [] val) :: Maybe String)
  let  cmd = ((B.lookup "what" $ fromMaybe [] val) :: Maybe String)
  let  chn = ((B.lookup "chan" $ fromMaybe [] val) :: Maybe String)
  let  whn = ((B.lookup "date" $ fromMaybe [] val) :: Maybe UTCTime)
  maybe (return $ n ++ unwords [":", nick, "means nothing to me."])
        (\msg -> return $ n ++ concat [": ", formatSeen nick msg (maybe "" id cmd) chn,
                          maybe "" ((' ' :) . timeAgo now) whn]) txt

formatSeen nick msg "PRIVMSG" (Just chan)
    | "\SOHACTION" `isPrefixOf` msg = concat [nick, " was all like *", nick, " ",
                                              U.excerpt 60 "..." . init . drop 8 $ U.trim msg, "* in ", chan]
    | otherwise                     = concat [nick, " said \"",
                                              U.excerpt 60 "..." $ U.trim msg,
                                              "\" in ", chan]

formatSeen nick msg "PART" (Just chan) = unwords [nick, "left", chan, "with the message:", msg]
formatSeen nick _   "JOIN" (Just chan) = unwords [nick, "joined", chan]
formatSeen nick msg "QUIT" _           = unwords [nick, "quit with the message:", msg]
formatSeen nick msg "NICK" _           = unwords [nick, "changed nick to", msg]
formatSeen _    _   _      _           = "did something unspeakable"

secret chan | chan `elem` secretChans = "#SECRET"
            | otherwise               = chan
            where secretChans = ["#jquery-ot"]

timeAgo now before = concatTime . relTime . round $ diffUTCTime now before

relTime t | t <  s     = ["now"]
          | t == s     = ["1 second"]
          | t <  m     = [show t ++ " seconds"]
          | t == m     = ["1 minute"]
          | t <  m * 2 = ["1 minute"]            ++ rest m
          | t <  h     = [first m ++ " minutes"] ++ rest m
          | t == h     = ["1 hour"]
          | t <  h * 2 = ["1 hour"]              ++ rest h
          | t <  d     = [first h ++ " hours"]   ++ rest h
          | t == d     = ["1 day"]
          | t <  d * 2 = ["1 day"]               ++ rest d
          | t <  w     = [first d ++ " days"]    ++ rest d
          | t == w     = ["1 week"]
          | t <  w * 2 = ["1 week"]              ++ rest w
          | t <  w * 4 = [first w ++ " weeks"]   ++ rest w
          | otherwise  = ["a long time"]
          where first  = show . div t
                rest v | t `mod` v == 0 = []
                       | otherwise      = relTime (mod t v)
                s = 1
                m = s * 60
                h = m * 60
                d = h * 24
                w = d * 7

concatTime xss@(x:_) | x == "now"      = x
                     | length xss == 1 = concat xss ++ " ago"
                     | otherwise       = intercalate ", " (init xss)
                                         ++ " and " ++ last xss ++ " ago"

concatTime [] = []

rollDie :: State StdGen Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

{-
punch :: Message -> String
punch (Message (Just (NickName nick _ _)) _ params@(p:ps)) = "\SOHACTION punches " ++ hahaha ++ " in the face!\SOH"
      where chan   = head ps
            text   = last ps
            target = takeWhile (/= ' ') . dropWhile (== ' ') . dropWhile (/= ' ') $ text
            hahaha = if target == "ultror" then nick else target
-}