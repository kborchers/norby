module IRC (
  module Parser,
  Net,
  connect,
  listen,
  privmsg,
  socket,
  write
) where

import           Control.OldException (bracket_)
import           Control.Monad
import           Control.Monad.Reader          hiding (join)
import           Data.Either
import           Data.List
import qualified Eval as E
import           Network
import           Parser
import qualified Seen as S
import           System.Exit
import           System.IO
import           Text.ParserCombinators.Parsec hiding (letter)
import           Types

import qualified Utils as U

-- A wrapper over IO, holding the bot's immutable state
type Net = ReaderT Bot IO
data Bot = Bot Handle

socket :: Bot -> Handle
socket (Bot h) = h

-- Connect to the server and return the initial bot state
connect :: String -> Int -> IO Bot
connect s p = notify $ do
    h <- connectTo s $ PortNumber (fromIntegral p)
    hSetBuffering h NoBuffering
    return $ Bot h
    where notify a = bracket_
                    (print ("Connecting to " ++ s ++ "...") >> hFlush stdout)
                    (print "It is so.") a

-- write $ Message (Maybe ett prefix) Command [parametrar]
-- write $ Message Nothing "LOL" ["nyeyhehe"]
write :: Message -> Net ()
write msg = do
    h <- asks socket
    liftIO $ hPrint h msg
    liftIO $ S.store  msg
    liftIO . putStrLn $ "sent: " ++ (show msg)

-- Prettier ones for later use :)
-- write msg = liftIO . flip hPrint msg =<< asks socket
-- write = (asks socket >>=) . (liftIO .) . flip hPrint

-- Process lines from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    let Just msg = parseMessage s -- Uh oh!
    liftIO . putStrLn $ "got:  " ++ s
    liftIO $ S.store msg -- Store every message in MongoDB
    ping msg
    where ping (Message _ "PING" p) = write $ Message Nothing "PONG" p
          ping message              = eval message

-- Perform a command
eval :: Message -> Net ()
eval msg@(Message _ _ params@(p:_))
   | ".join "  `isPrefixOf` lastPar = write $ Message Nothing "JOIN"
                                              (take 1 . drop 1 $ words lastPar)
   | ".part "  `isPrefixOf` lastPar = write $ Message Nothing "PART"
                                              (take 1 . drop 1 $ words lastPar)
   | "> "      `isPrefixOf` lastPar = eval' E.evalHsExt msg
   | ".type "  `isPrefixOf` lastPar = eval' E.typeOf msg
   | ".gtfo "  `isPrefixOf` lastPar = write $ Message Nothing "QUIT" ["lol haahaha!"]
   | ".seen "  `isPrefixOf` lastPar = eval' S.seen msg
   | ".pf "    `isPrefixOf` lastPar = eval' E.pointFree msg
   | otherwise                      = return ()
   where eval' f msg = liftIO (f msg) >>= privmsg chan
         lastPar     = last params
         chan        = p

eval (Message _ _ _) = return ()

privmsg :: String -> String -> Net ()
privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]
