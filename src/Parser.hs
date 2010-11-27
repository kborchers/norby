module Parser (parseMessage) where

import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (letter, space)
import           Types

parseMessage :: String -> Maybe Message
parseMessage s = either (const Nothing) Just (parse message "Message" s)

message :: CharParser () Message
message = do
  p  <- optionMaybe (char ':' >> prefix >>= (space >>) . return)
  c  <- command
  ps <- many (space >> params)
  return $ Message p c ps

prefix   :: CharParser () Prefix
prefix   = try nickPrefix <|> serverPrefix

command  :: CharParser () Command
command  = many1 letter <|> count 3 number -- Either one or more letters, or three numbers

space :: CharParser () String
space = many1 $ char '\SP'

params :: CharParser () Param
params = optionMaybe ((char ':' >> trailing) <|> middle) >>= return . fromMaybe ""

middle :: CharParser () String
middle = many1 nonWhite

trailing :: CharParser () String
trailing = many $ noneOf "\NUL\CR\LF"

host :: CharParser () String
host = many1 $ noneOf " @!" -- Get a real one

nick :: CharParser () String
nick = many1 (letter <|> number <|> special)

nickPrefix :: CharParser () Prefix
nickPrefix = do
  n <- nick
  _ <- notFollowedBy $ char '.'
  u <- optionMaybe (char '!' >> user)
  h <- optionMaybe (char '@' >> host)
  return $ NickName n u h

serverName   :: CharParser () String
serverName   = host

serverPrefix :: CharParser () Prefix
serverPrefix = serverName >>= return . Server

user     :: CharParser () String
user     = many1 $ noneOf "\SP\NUL\CR\LF\64"

letter   :: CharParser () Char
letter   = oneOf $ ['a'..'z'] ++ ['A'..'Z']

number   :: CharParser () Char
number   = oneOf ['0'..'9']

special  :: CharParser () Char
special  = oneOf "-[]\\`^{}_|" -- Added "_|", because IRL /= RFC

nonWhite :: CharParser () Char
nonWhite = noneOf "\SP\NUL\CR\LF"
