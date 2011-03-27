-- | Utilities for great justice
{-# Language OverloadedStrings #-}
module Utils where

import Data.Char
import Text.Printf

-- * String utilities
trim :: String -> String
trim = trim' . trim'
       where trim' = reverse . dropWhile isSpace

excerpt :: Int -> String -> String -> String
excerpt len end s | null $ drop len s = s -- Don't check the length of `s`!
                  | otherwise         = take (len - length end) s ++ end

excerpt' :: String -> String
excerpt' s = excerpt 225 "..." s

relTime :: Int -> String
relTime t = printTime . take 3 $ omg t times
    where omg _ [] = []
          omg t ((x, s):xs)
              | divs == 0 = rest
              | divs == 1 = (divs, s) : rest
              | otherwise = (divs, s ++ "s") : rest
              where divs = div t x
                    rest = omg (mod t x) xs
          times = [ (31556926, "year")
                  , (2629744,  "month")
                  , (604800,   "week")
                  , (86400,    "day")
                  , (3600,     "hour")
                  , (60,       "minute")
                  , (1,        "second")
                  ]

printTime :: (PrintfArg a, Integral a) => [(a, String)] -> String
printTime xs = case xs of
    []                   -> []
    [(n, s)]             -> printf "%d %s ago" n s
    [(n1, s1), (n2, s2)] -> printf "%d %s and %d %s ago" n1 s1 n2 s2
    ((n, s):xs)          -> printf "%d %s, %s" n s (printTime xs)
