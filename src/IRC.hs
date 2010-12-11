module IRC (
  Net,
  connect,
  listen,
  privmsg,
  socket,
  write
) where

import           Control.Exception (bracket_)
import           Control.Monad.Reader hiding (join)
import           Data.List
import qualified Eval as E
import           Network
import           Parser
import qualified Seen as S
import           Settings
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

write :: Message -> Net ()
write msg = asks socket >>= \h -> liftIO $ hPrint h (encode msg)
                        >> S.store msg >> putStrLn ("sent: " ++ (encode msg))

-- Process lines from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- fmap init . liftIO $ hGetLine h
    let Just msg = decode s -- Uh oh! NON-EXHAUSTIVE PATTERNS
    _ <- liftIO ((putStrLn $ "got:  " ++ s) >> S.store msg) -- Store every message in MongoDB
    eval msg

-- Decide what to do
eval :: Message -> Net ()
eval (Message _ "PING" p) = write $ Message Nothing "PONG" p
eval msg@(Message (Just (NickName nn _ _)) _ ps@(p:_))
   | match ".join"  = join (sndWord lastParam)
   | match ".part"  = part (sndWord lastParam)
   | match ".gtfo"  = quit ["LOL"]
   | match ">"      = command E.evalHsExt
   | match ".type"  = command E.typeOf
   | match ".seen"  = command S.seen
   | match ".pf"    = command E.pointFree
   | match ".unpf"  = command E.pointFul
   | otherwise      = return ()
   where command f  = liftIO (f msg) >>= privmsg target
         lastParam  = last ps
         match s    = (s ++ " ") `isPrefixOf` lastParam
         target | p == nick = nn
                | otherwise = p

eval (Message _ _ _) = return ()

privmsg :: Param -> Param -> Net ()
privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]

join :: Params -> Net ()
join = write . Message Nothing "JOIN"
part :: Params -> Net ()
part = write . Message Nothing "PART"
quit :: Params -> Net ()
quit = write . Message Nothing "QUIT"

sndWord :: String -> [String]
sndWord = take 1 . drop 1 . words
