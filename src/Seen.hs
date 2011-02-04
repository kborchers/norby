{-# Language OverloadedStrings #-}

module Seen where

import           Commands
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

store :: Message -> Net ()
store (Message (Just (NickName nick _ _)) cmd params) = do
    now <- liftIO getCurrentTime
    cp  <- asks pool
    let mess = last params
    let chan = head params
    let message = [ "nick" =: map toLower nick
                  , "text" =: mess, "what" =: cmd
                  , "chan" =: chan, "date" =: now ]
    runDb cp $ insert collection message
    return ()
store _ = return ()

seen :: Message -> Net ()
seen (Message (Just (NickName n _ _)) _ params)
    | ln    == lnick  = privmsg target $ printf "%s: That's you, right there." n
    | lnick == S.nick = privmsg target $ printf "%s: That's me." n
    | otherwise       = do
        cp <- asks pool
        qr <- runDb cp $ findNick lnick
        case qr of
                (Left _)  -> privmsg target "Connection error."
                (Right v) -> result v
    where findNick n  = findOne (select [ "nick" =: n ] collection) { sort = [ "_id" =: (-1 :: Int) ]}
          timeAgo d m = U.relTime . round $ diffUTCTime d m
          result Nothing  = privmsg target $ printf
                            "%s: I have never seen %s." n nick
          result (Just d) = do
              now <- liftIO getCurrentTime
              let txt = B.at "text" d
              let cmd = B.at "what" d
              let chn = B.at "chan" d
              let whn = B.at "date" d
              privmsg target $ printf "%s: %s was seen %s, %s"
                                      n nick (timeAgo now whn)
                                      (formatSeen txt cmd chn)
          ln    = map toLower n
          lnick = map toLower nick
          nick  = U.trim . concat . take 1 . drop 1 . words . last $ params
          chan  = concat $ take 1 params
          target | chan == S.nick = n
                 | otherwise      = chan

seen _ = return ()

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
