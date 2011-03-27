module Messages where

import Control.Monad.Reader (asks, liftIO)
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
