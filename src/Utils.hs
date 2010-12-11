-- | Utilities for great justice
module Utils where

import           Data.Char
--import           Data.List

-- * String utilities
trim :: String -> String
-- ^ Remove leading and trailing whitespace
trim = trim' . trim'
       where trim' = reverse . dropWhile isSpace

excerpt :: Int -> String -> String -> String
-- ^ Truncate string to given length, and end with a string
excerpt len end s | null $ drop len s = s -- Don't check the length of `s`!
                  | otherwise         = take (len - length end) s ++ end

excerpt' :: String -> String
excerpt' = excerpt 225 "..."
