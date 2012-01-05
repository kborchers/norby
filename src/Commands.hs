{-# Language OverloadedStrings #-}
module Commands where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import Data.Char
import Data.List (intercalate, isPrefixOf)
import Eval
import Messages
import Seen as S
import Settings
import System.Exit
import Types
import Utils

eval :: Message -> Net ()
eval msg = sequence_ $ fmap ($ msg) handlers

handlers = [ connectHandler
           , evalHandler
           , inviteHandler
           , joinHandler
           , logHandler
           , partHandler
           , pointFreeHandler
           , pointFulHandler
           , pingHandler
           , quitHandler
           , sayHandler
           , seenHandler
           , typeHandler
           ]

-- These seem very repetitive, but at least nicer than the old stuff
-- The pattern matching should be replaced by a predicate function
-- so that the handler just needs to do its thing
connectHandler msg = case msg of
    (Message _ "001" _) -> join [intercalate "," channels]
    _                   -> return ()

evalHandler msg = case msg of
   msg@(Message (Just (NickName _ _ _)) _ ps@(p:_))
       -> when ("> " `isPrefixOf` last ps)
               (liftIO (evalHsExt msg) >>= replyTo msg)
   _   -> return ()

inviteHandler msg = case msg of
    (Message (Just (NickName _ _ _)) "INVITE" ps)
        -> join $ drop 1 ps
    _   -> return ()

joinHandler msg = case msg of
    (Message (Just (NickName nn _ _)) _ ps)
        -> when (".join " `isPrefixOf` last ps && nn `elem` admins)
                (join . drop 1 . words $ last ps)
    _   -> return ()

logHandler msg  = S.store msg

partHandler msg = case msg of
    (Message (Just (NickName nn _ _)) _ ps)
        -> when (".part " `isPrefixOf` last ps && nn `elem` admins)
                (part . sndWord $ last ps)
    _   -> return ()

pingHandler msg = case msg of
    (Message _ "PING" p) -> write $ Message Nothing "PONG" p
    _                    -> return ()

pointFreeHandler msg = case msg of
    (Message (Just (NickName _ _ _)) _ ps@(p:_))
      -> when (".pf " `isPrefixOf` last ps)
              (liftIO (pointFree msg) >>= replyTo msg)
    _ -> return ()

pointFulHandler msg = case msg of
    (Message (Just (NickName _ _ _)) _ ps@(p:_))
      -> when (".unpf " `isPrefixOf` last ps)
              (liftIO (pointFul msg) >>= replyTo msg)
    _ -> return ()

sayHandler msg = case msg of
    (Message (Just (NickName nn _ _)) _ ps)
      -> when (".say " `isPrefixOf` last ps && nn `elem` admins)
              (privmsg target message)
              where target    = takeWhile (not . isSpace) . skipSpace . skipWord $ last ps
                    message   = skipSpace . skipWord . skipSpace . skipWord $ last ps
                    skipWord  = dropWhile (not . isSpace)
                    skipSpace = dropWhile isSpace
    _ -> return ()

seenHandler msg = case msg of
    (Message (Just (NickName _ _ _)) _ ps)
      -> when (".seen " `isPrefixOf` last ps) (S.seen msg)
    _ -> return ()

quitHandler msg = case msg of
    (Message (Just (NickName nn _ _)) _ ps)
      -> when (".quit " `isPrefixOf` lastp && nn `elem` admins)
              (quit quitMsg >> liftIO exitSuccess)
              where quitMsg = drop 1 $ words lastp
                    lastp   = last ps
    _ -> return ()

typeHandler msg = case msg of
    (Message (Just (NickName _ _ _)) _ ps@(p:_))
      -> when (".type " `isPrefixOf` last ps)
              (liftIO (typeOf msg) >>= replyTo msg)
    _ -> return ()
