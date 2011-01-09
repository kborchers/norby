module Commands where

import           Control.Monad.Reader
--import qualified Seen as S
import           System.IO
import           Types
import qualified Utils as U

-- Decide what to do
{-
eval :: Message -> Net ()
eval (Message _ "PING" p) = write $ Message Nothing "PONG" p
eval msg@(Message (Just (NickName nn _ _)) _ ps@(p:_))
   | match ".join"  = join (sndWord lastParam)
   | match ".part"  = part (sndWord lastParam)
   | match ">"      = command E.evalHsExt
   | match ".type"  = command E.typeOf
   | match ".seen"  = command S.seen
   | match ".pf"    = command E.pointFree
   | match ".unpf"  = command E.pointFul
   | match ".gtfo"  = quit ["LOL"] >> liftIO exitSuccess
   | otherwise      = return ()
   where command f  = liftIO (f msg) >>= privmsg target
         lastParam  = last ps
         match s    = (s ++ " ") `isPrefixOf` lastParam
         target | p == nick = nn
                | otherwise = p

eval (Message _ _ _) = return ()
-}
write :: Message -> Net ()
write msg = do
    h <- asks socket
    liftIO . hPutStrLn h $ encode msg
    liftIO . putStrLn $ "sent: " ++ encode msg
    --liftIO $ S.store msg

privmsg :: Param -> Param -> Net ()
privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]
