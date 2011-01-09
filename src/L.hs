{-# LANGUAGE NoMonomorphismRestriction #-}

module L where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Data.Word
import           Network.URI
import           Numeric
import           Text.Printf

interleave []     _  = []
interleave (x:xs) ys = x : interleave ys xs

padl n p s = pad n p s ++ s
padr = (ap (++) .) . pad
pad  = (flip (drop . length) .) . replicate

rgbToHex :: Word8 -> Word8 -> Word8 -> String
rgbToHex r g b = '#' : concatMap (padl 2 '0' . flip showHex "") [r, g, b]

-- temp01's magical function
oOo [] = []
oOo s  = concat [init s, [toUpper (last s)], tail (reverse s)]

type Peen = String
ben :: Integer -> Peen
ben = printf "8%sD" . flip genericReplicate '='

fap = fmap

gf :: Integer -> String
gf n | n <  0    = "NEGATIVE U"
     | n == 0    = "N'T U"
     | n <= 9000 = printf "%s U" (unwords $ genericReplicate n "NO")
     | otherwise = "It's over 9000!"

ajpiano = "PANDEMONIUM!!!" :: String

akahn :: Integer -> String
akahn 0 = "akahn :)"
akahn n = printf "AK%sHN!!" (genericReplicate n 'A')

coldhead :: String -> String
coldhead  s | null s    = ">: |"
            | otherwise = printf "these are truly the last %s" s

dabear = printf "your mom %s" :: String -> String

dytrivedi :: String -> String
dytrivedi s | null s    = "my wife is happy"
            | otherwise = mappend "my wife is annoyed i spend so much time " s

matjas = interleave "matjas" . enumFrom :: Char -> String

miketaylr :: String -> String
miketaylr s | null ts   = "here, let me open that... wait a minute, there's nothing there, you bitch!"
            | otherwise = unwords ["here, let me open that", s, "for you!"]
            where ts = trim s

nlogax     = printf "%sn't"           :: String -> String
paul_irish = ($)                      :: (effin -> rad) -> effin -> rad
rwaldron   = printf "%s. Questions?"  :: String -> String
sean       = "koole"                  :: String
seutje     = printf "I would have %s" :: String -> String
temp01     = Just "awesome"           :: Maybe String
vladikoff  = printf "flod %s!!"       :: String -> String

mlu = "much like urself"
muu = "much unlike urself"

trim = let t = reverse . dropWhile isSpace in t . t

dropInit = drop =<< subtract 1 . length

relTime :: (Integral a, PrintfArg a) => a -> String
relTime = printTime . take 3 . flip omg times
    where omg _ []     = []
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

printTime :: (PrintfArg t, PrintfArg t1) => [(t, t1)] -> String
printTime []                   = []
printTime [(n, s)]             = printf "%d %s" n s
printTime [(n1, s1), (n2, s2)] = printf "%d %s and %d %s" n1 s1 n2 s2
printTime ((n, s):xs)          = printf "%d %s, %s" n s (printTime xs)

-- Omg postfix function application so you can `car & drive` instead of `drive car`!
infixl 0 &
x & f = f x

ftoc t = 5/9 * (t - 32)
ctof t = (9/5 * t) + 32