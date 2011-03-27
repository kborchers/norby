import Bot
import Control.Exception
import Control.Monad.Reader
import Prelude              hiding (catch)
import System.IO

import Settings

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket (connect server port) disconnect loop
       where disconnect = hClose . socket
             loop st    = (catch :: IO a -> (IOException -> IO a) -> IO a)
                          (runReaderT run st) (const $ return ())
