module Types where

import           Data.List

data Message = Message (Maybe Prefix) Command Params
               deriving (Eq, Read)

-- The (optional) prefix can be either a servername or a nickname
-- with optional username and host
data Prefix  = Server ServerName
             | NickName String (Maybe UserName) (Maybe ServerName)
               deriving (Eq, Read)

type Command    = String
type Param      = String
type Params     = [Param]
type RealName   = String
type ServerName = String
type UserName   = String

-- HMMM: Is this bad use of the Show class?
instance Show Message where
         show (Message (Just prefix) command params) =
               intercalate " " [show prefix, command,
                                paramize params]
         show (Message Nothing command params) =
               intercalate " " [command, paramize params]

instance Show Prefix where
         show (Server s) = ':' : s
         show (NickName nick user server) = ':' : nick
                              ++ maybe "" ('!' :) user
                              ++ maybe "" ('@' :) server

paramize :: Params -> String
paramize []  = ""
paramize [x] = ':' : x
paramize ps  = intercalate " " (init ps) ++ " :" ++ last ps
