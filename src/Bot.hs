{-# Language OverloadedStrings #-}
module Bot (
  Net,
  connect,
  listen,
  run,
  socket,
  write
) where

import Commands                    (eval)
import Control.Concurrent
import Control.Exception           (bracket_)
import Control.Monad.Reader hiding (join)
import Database.MongoDB     hiding (eval)
import Messages
import Network
import Parser
import Settings
import System.IO
import Types

-- Connect to the server and return the initial bot state
connect :: String -> Int -> IO Bot
connect s p = notify $ do
    handle <- connectTo s . PortNumber $ fromIntegral p
    -- Create a connection pool that can be used in any Bot context
    cPool  <- newConnPool 8 $ host "127.0.0.1"
    hSetBuffering handle NoBuffering
    return $ Bot handle cPool
    where notify a = bracket_
                    (print ("Connecting to " ++ s ++ "...") >> hFlush stdout)
                    (print "Done.") a

-- Join some channels, and start processing commands
run :: Net ()
run = mapM_ write msgs >> asks socket >>= listen
      where msgs = [ Message Nothing "NICK" [nick]
                   , Message Nothing "USER" [nick, "0", "*", name]
                   ]

{-
How can i fork a ReaderT r IO ()?
forkIO (runReaderT m state)
If in the ReaderT: state <- ask; liftIO . forkIO $ runReaderT m state
-}

-- Process lines from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- fmap init . liftIO $ hGetLine h
    liftIO . putStrLn $ "got:  " ++ s
    let Just msg = decode s -- Uh oh! NON-EXHAUSTIVE PATTERNS
    -- Handle each message in a new thread
    liftIO . forkIO . runReaderT (eval msg) =<< ask
