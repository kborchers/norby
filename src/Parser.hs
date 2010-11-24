module Parser (parseMessage) where

import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (letter, space)
import           Types

parseMessage :: String -> Maybe Message
parseMessage s = either (const Nothing) Just (parse message "Message" s)

message = do
  p  <- optionMaybe (char ':' >> prefix >>= (space >>) . return)
  c  <- command
  ps <- many (space >> params)
  return $ Message p c ps

prefix   = try nickPrefix <|> serverPrefix
command  = many1 letter <|> count 3 number -- Either one or more letters, or three numbers
space    = many1 $ char '\SP'
params   = optionMaybe ((char ':' >> trailing) <|> middle) >>= return . fromMaybe ""
middle   = many1 nonWhite
trailing = many $ noneOf "\NUL\CR\LF"

crlf = string "\CR\LF"
host = many1 $ noneOf " @!"
nick = many1 (letter <|> number <|> special)

nickPrefix = do
  n <- nick
  _ <- notFollowedBy $ char '.'
  u <- optionMaybe (char '!' >> user)
  h <- optionMaybe (char '@' >> host)
  return $ NickName n u h

serverName   = host
serverPrefix = serverName >>= return . Server

user     = many1 $ noneOf "\SP\NUL\CR\LF\64"
letter   = oneOf $ ['a'..'z'] ++ ['A'..'Z']
number   = oneOf ['0'..'9']
special  = oneOf "-[]\\`^{}_|" -- Added "_|", because IRL /= RFC
nonWhite = noneOf "\SP\NUL\CR\LF"
