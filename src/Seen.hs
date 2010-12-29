{-# Language OverloadedStrings #-}

module Seen where

import           Control.Monad.Reader
import qualified Data.Bson as B
import           Data.Char
import           Data.List hiding (sort, insert)
import           Data.Time
import           Database.MongoDB hiding (rest)
import           Network.Abstract
import qualified Settings as S
import           Text.Printf
import           Types

import qualified Utils as U

collection :: Collection
collection = "messages"

run :: (NetworkIO m) => ReaderT Database (Action m) a -> m (Either Failure a)
run action = do
    pool <- newConnPool 1 $ host "127.0.0.1"
    access safe Master pool $ use (Database "seen") action

store :: Message -> IO ()
store (Message (Just (NickName nick _ _)) cmd params) = do
      now <- getCurrentTime
      let mess = last params
      let chan = head params
      let message = [ "nick" =: map toLower nick
                    , "text" =: mess, "what" =: cmd
                    , "chan" =: chan, "date" =: now ]
      run (insert collection message)
      return ()
store (Message _ _ _) = return ()

seen :: Message -> IO String
seen (Message (Just (NickName n _ _)) _ params)
    | ln    == lnick  = return $ printf "%s: That's you, I see you in %s right now." n chan
    | lnick == S.nick = return $ printf "%s: That's me, I am here in %s." n chan
    | otherwise       = do a <- run (findNick nick)
                           case a of
                                (Right v) -> result v
                                _         -> return "DOOOOM"
     where ln    = map toLower n
           lnick = map toLower nick
           findNick nn =
                    findOne (select [ "nick" =: map toLower nn ] collection)
                            { sort = [ "_id" =: (-1 :: Int) ] }
           
           result (Just v) = do
                  now <- getCurrentTime
                  let txt = B.at "text" v :: String
                  let cmd = B.at "what" v :: String
                  let chn = B.at "chan" v :: String
                  let whn = B.at "date" v :: UTCTime
                  return $ printf "%s: %s was seen %s, %s" n nick (timeAgo now whn)
                                                           (formatSeen txt cmd chn)
           
           result Nothing = return $
                            printf "%s: I have never seen %s." n nick
           timeAgo d m = relTime . round  $ diffUTCTime d m
           nick = U.trim . concat . take 1 . drop 1 . words . last $ params
           chan = concat $ take 1 params

seen (Message _ _ _) = return "NO U"

formatSeen :: String -> String -> String -> String
formatSeen msg "PRIVMSG" chan
    | "\SOHACTION" `isPrefixOf` msg = printf "in %s, actioning *%s*" chan
                                             (U.excerpt 150 "..." . init . drop 8 $ U.trim msg)
    | otherwise = printf "in %s, saying: %s" chan
                         (U.excerpt 150 "..." $ U.trim msg)

formatSeen m cmd c = case cmd of
    "PART" -> printf "leaving %s" c
    "JOIN" -> printf "joining %s" c
    "QUIT" -> printf "quitting with the message: %s" m'
    "NICK" -> printf "changing nick to %s" m
    _      -> printf "doing something unspeakable"
    where m' = U.excerpt' $ U.trim m

relTime :: Int -> String
relTime = printTime . take 3 . flip omg times
    where omg _ []     = []
          omg t ((x, s):xs)
              | divs == 0 = rest
              | divs == 1 = (divs, s) : rest
              | otherwise = (divs, s ++ "s") : rest
              where divs = div t x
                    rest = omg (mod t x) xs
          times = [ (31556926, "year")
                  , (2629744,  "month")
                  , (604800,   "week")
                  , (86400,    "day")
                  , (3600,     "hour")
                  , (60,       "minute")
                  , (1,        "second")
                  ]

printTime :: (PrintfArg a, Integral a) => [(a, String)] -> String
printTime []                   = []
printTime [(n, s)]             = printf "%d %s" n s
printTime [(n1, s1), (n2, s2)] = printf "%d %s and %d %s" n1 s1 n2 s2
printTime ((n, s):xs)          = printf "%d %s, %s" n s (printTime xs)
