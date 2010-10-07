module Parser (parseMessage) where

import           Data.Maybe
import           Text.ParserCombinators.Parsec hiding (letter, space)
import           Types

parseMessage :: String -> Maybe Message
parseMessage s = either (const Nothing) Just (parse message "" s)

-- <message> ::=  [':' <prefix> <SPACE> ] <command> <params> <crlf>
message = do
  p  <- optionMaybe (char ':' >> prefix >>= (space >>) . return)
  c  <- command
  ps <- many (space >> params)
--  _  <- crlf
  return $ Message p c ps
-- <prefix> ::=   <servername> | <nick> [ '!' <user> ] [ '@' <host> ]
prefix = try nickPrefix <|> serverPrefix

-- <command> ::=  <letter> { <letter> } | <number> <number> <number>
command  = many1 letter <|> count 3 number -- Either one or more letters, or three numbers
-- <SPACE> ::=    ' ' { ' ' }
space    = many1 $ char '\SP'
-- <params> ::=   <SPACE> [ ':' <trailing> | <middle> <params> ]
params = optionMaybe ((char ':' >> trailing) <|> middle) >>= return . fromMaybe ""
-- <middle> ::=   <Any *non-empty* sequence of octets not including SPACE or NUL or CR or LF, the first of which may not be ':'>
middle = many1 nonWhite

-- <trailing> ::= <Any, possibly *empty*, sequence of octets not including NUL or CR or LF>
trailing = many $ noneOf "\NUL\CR\LF"
-- <crlf> ::=     CR LF
crlf     = string "\CR\LF"

-- <host> ::=       see RFC 952 [DNS:4] for details on allowed hostnames
host     = many1 $ noneOf " @!"

-- <nick> ::=       <letter> { <letter> | <number> | <special> }
-- This is more liberal than the RFC, to make it work IRL
nick = many1 (letter <|> number <|> special)

nickPrefix = do
  n <- nick
  _ <- notFollowedBy $ char '.'
  u <- optionMaybe (char '!' >> user)
  h <- optionMaybe (char '@' >> host)
  return $ NickName n u h


serverName = host
serverPrefix = serverName >>= return . Server

-- <user> ::=     <nonwhite> { <nonwhite> }
user    = many1 $ noneOf "\SP\NUL\CR\LF\64"

-- <letter> ::=   'a' ... 'z' | 'A' ... 'Z'
letter  = oneOf $ ['a'..'z'] ++ ['A'..'Z']
-- <number> ::=   '0' ... '9'
number  = oneOf ['0'..'9']
-- <special> ::=  '-' | '[' | ']' | '\' | '`' | '^' | '{' | '}'
special = oneOf "-[]\\`^{}_|" -- Added "_|", because IRL /= RFC

-- <nonwhite> ::= <any 8bit code except SPACE (0x20), NUL (0x0), CR (0xd), and LF (0xa)>
nonWhite = noneOf "\SP\NUL\CR\LF"
