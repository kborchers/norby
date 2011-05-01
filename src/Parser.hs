module Parser (decode) where

import Data.Maybe
import Text.ParserCombinators.Parsec hiding (letter, space)
import Types

decode :: String -> Maybe Message
decode s = either (const Nothing) Just (parse message "Message" s)

message = do
    p  <- optionMaybe (char ':' >> prefix >>= (space >>) . return)
    c  <- command
    ps <- many (space >> params)
    return $ Message p c ps

nickPrefix = do
    n <- nick
    _ <- notFollowedBy $ char '.'
    u <- optionMaybe (char '!' >> user)
    h <- optionMaybe (char '@' >> host)
    return $ NickName n u h

command       = many1 letter <|> count 3 number -- Either one or more letters, or three numbers
host          = many1 $ noneOf " @!" -- TODO: write a real one
letter        = oneOf $ ['a'..'z'] ++ ['A'..'Z']
middle        = many1 nonWhite
nick          = many1 (letter <|> number <|> special)
nonWhite      = noneOf "\SP\NUL\CR\LF"
number        = oneOf ['0'..'9']
params        = optionMaybe ((char ':' >> trailing) <|> middle) >>= return . fromMaybe ""
prefix        = try nickPrefix <|> serverPrefix
serverName    = host
serverPrefix  = serverName >>= return . Server
space         = many1 $ char '\SP'
special       = oneOf "-[]\\`^{}_|" -- Added "_|" in addition to the ones in the RFC
trailing      = many  $ noneOf "\NUL\CR\LF"
user          = many1 $ noneOf "\SP\NUL\CR\LF\64"
