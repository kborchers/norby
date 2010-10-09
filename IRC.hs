module IRC (
  Net,
  connect,
  listen,
  privmsg,
  socket,
  write
) where

import           Control.OldException (bracket_)
import           Control.Monad        hiding (join)
import           Control.Monad.Reader hiding (join)
import           Data.Either
import           Data.List
import qualified Eval as E
import           Network
import           Parser
import qualified Seen as S
import           Settings
import           System.Exit
import           System.IO
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
eval msg@(Message (Just (NickName nn _ _)) _ ps@(p:_))
   | ".join "  `isPrefixOf` lastp = join (sndWord lastp)
   | ".part "  `isPrefixOf` lastp = part (sndWord lastp)
   | ".gtfo "  `isPrefixOf` lastp = quit ["LOL"]
   | "> "      `isPrefixOf` lastp = eval' E.evalHsExt msg
   | ".type "  `isPrefixOf` lastp = eval' E.typeOf    msg
   | ".seen "  `isPrefixOf` lastp = eval' S.seen      msg
   | ".pf "    `isPrefixOf` lastp = eval' E.pointFree msg
   | ".unpf "  `isPrefixOf` lastp = eval' E.pointFul  msg
   | otherwise                    = return ()
   where eval' f msg = liftIO (f msg) >>= privmsg target
         lastp       = last ps
         sndWord     = take 1 . drop 1 . words
         target      = if p == nick then nn else p -- Ugly

eval (Message _ _ _) = return ()

privmsg :: Param -> Param -> Net ()
privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]

join ps = write $ Message Nothing "JOIN" ps

part ps = write $ Message Nothing "PART" ps

quit ps = write $ Message Nothing "QUIT" ps
