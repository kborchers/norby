module Messages where

import Control.Monad.Reader (asks, liftIO)
import Settings
import System.IO
import Types
import Utils as U

write :: Message -> Net ()
write msg = do
    h <- asks socket
    liftIO . hPutStrLn h $ encodedMsg
    liftIO . putStrLn $ "sent: " ++ encodedMsg
    where encodedMsg = encode msg

privmsg c m = write $ Message Nothing "PRIVMSG" [c, U.excerpt' m]

join = write . Message Nothing "JOIN"
part = write . Message Nothing "PART"
quit = write . Message Nothing "QUIT"

-- Convenience function to reply to the correct channel or person
replyTo msg reply = case msg of
    (Message (Just (NickName nn _ _)) _ (p:_))
        -> privmsg recip reply
           where recip = if p == nick then nn else p
    _   -> return ()
