{-# Language OverloadedStrings #-}
module Commands where

import           Control.Monad (when)
import           Control.Monad.Reader (asks, liftIO)
import           Data.List (isPrefixOf)
import           Eval
import qualified Seen as S
import           Settings
import           System.Exit
import           System.IO
import           Types
import qualified Utils as U

eval :: Message -> Net ()
eval msg = sequence_ $ fmap ($ msg) handlers

handlers = [ evalHandler
           , joinHandler
           , logHandler
           , partHandler
           , pingHandler
           , quitHandler
           , seenHandler
           ]

-- These seem very repetitive, but at least nicer than the old stuff
evalHandler msg = case msg of
   msg@(Message (Just (NickName nn _ _)) _ ps@(p:_))
       -> when ("> " `isPrefixOf` last ps)
               (liftIO (evalHsExt msg) >>= privmsg p)
   _   -> return ()

joinHandler msg = case msg of
    (Message (Just (NickName nn _ _)) _ ps)
        -> when (".join " `isPrefixOf` last ps && nn `elem` admins)
                (join . sndWord $ last ps)
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

-- Convenience function to reply to the correct channel or person
replyTo reply msg = case msg of
    (Message (Just (NickName nn _ _)) _ (p:_))
        -> privmsg recip reply
           where recip | p == nick = nn -- query
                       | otherwise = p  -- channel
    _   -> return ()

write :: Message -> Net ()
write msg = do
    h <- asks socket
    liftIO . hPutStrLn h $ encodedMsg
    liftIO . putStrLn $ "sent: " ++ encodedMsg
    S.store msg
    where encodedMsg = encode msg

privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]

join = write . Message Nothing "JOIN"
part = write . Message Nothing "PART"
quit = write . Message Nothing "QUIT"

sndWord = take 1 . drop 1 . words
