{-# Language OverloadedStrings #-}
module Types where

import Control.Monad.Reader
import Data.List
import Database.MongoDB hiding (Command)
import System.IO

data Bot = Bot Handle (ConnPool Host)
type Net = ReaderT Bot IO
-- ^ A wrapper over IO, holding the immutable state of the bot
socket :: Bot -> Handle
socket (Bot h _) = h

pool :: Bot -> ConnPool Host
pool (Bot _ cp) = cp

runDb :: (Service s, MonadIO m)
      => ConnPool s -> ReaderT Database (Action m) a -> m (Either Failure a)
runDb cp action = access safe Master cp
                $ use (Database "seen") action

data Message = Message (Maybe Prefix) Command Params
               deriving (Eq, Read, Show)

-- The (optional) prefix can be either a servername or a nickname
-- with optional username and host
data Prefix  = NickName String (Maybe UserName) (Maybe ServerName)
             | Server ServerName
               deriving (Eq, Read, Show)

type Command    = String
type Param      = String
type Params     = [Param]
type RealName   = String
type ServerName = String
type UserName   = String

encode :: Message -> String
encode (Message (Just prefix) command params) =
        intercalate " " [ penc prefix
                        , command
                        , paramize params
                        ]
        where penc (Server s) = ':' : s
              penc (NickName nick user server) = ':' : nick
                                   ++ maybe "" ('!' :) user
                                   ++ maybe "" ('@' :) server
encode (Message Nothing command params) =
        intercalate " " [ command
                        , paramize params ]

paramize :: Params -> String
paramize ps = case ps of
    []     -> []
    [x]    -> ':' : x
    (x:xs) -> unwords [x, paramize xs]
