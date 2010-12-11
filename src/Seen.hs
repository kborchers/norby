{-# Language OverloadedStrings #-}

module Seen where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.Bson as B
import           Data.Char
import           Data.List hiding (sort, insert)
import           Data.Time
import           Database.MongoDB hiding (rest)
import qualified Settings as S
import           Text.Printf
import           Types

import qualified Utils as U

collection :: Collection
collection = "messages"

run :: (MonadIO m, Applicative m)
    => ReaderT Database (Action m) a -> m (Either Failure a)
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
      run (insert collection message) >> return ()

store (Message _ _ _) = return ()

seen :: Message -> IO String
seen (Message (Just (NickName n _ _)) _ params)
    | n    == nick   = return $ printf "%s: That's you, I see you in %s right now." n chan
    | nick == S.nick = return $ printf "%s: That's me, I am here in %s." n chan
    | otherwise      = do
              a <- run (findNick nick)
              case a of
                   (Right v) -> result v
                   _         -> return "DOOOOM"
     
     where findNick nn =
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
           timeAgo = ((concatTime . relTime . round) .) . diffUTCTime
           nick = U.trim . concat . take 1 . drop 1 . words . last $ params
           chan = concat $ take 1 params

seen (Message _ _ _) = return "nlogax fails at pattern matching."

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

relTime :: Int -> [String]
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

concatTime :: [String] -> String
concatTime []        = []
concatTime xss@(x:_) | x == "now"      = x
                     | 1 == length xss = printf "%s ago" $ concat xss
                     | otherwise       = printf "%s and %s ago"
                                                (intercalate ", " $ init xss)
                                                                  $ last xss
