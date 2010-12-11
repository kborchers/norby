module Types where

import           Data.List

data Message = Message (Maybe Prefix) Command Params
               deriving (Eq, Read, Show)

-- The (optional) prefix can be either a servername or a nickname
-- with optional username and host
data Prefix  = Server ServerName
             | NickName String (Maybe UserName) (Maybe ServerName)
               deriving (Eq, Read, Show)

type Command    = String
type Param      = String
type Params     = [Param]
type RealName   = String
type ServerName = String
type UserName   = String

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
paramize []  = ""
paramize [x] = ':' : x
paramize ps  = intercalate " " (init ps) ++ " :" ++ last ps
