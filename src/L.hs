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
import qualified Data.Text as T
import           Network.URI
import           Numeric
import           Text.PrettyPrint.HughesPJ
import           Text.Printf
import           Text.Regex

interleave []     _  = []
interleave (x:xs) ys = x : interleave ys xs

padl n p s = pad n p s ++ s
padr = (ap (++) .) . pad
pad  = (flip (drop . length) .) . replicate

rgbToHex (r, g, b)
         | all ok rgb = '#' : concatMap (padl 2 '0' . flip showHex "") rgb
         | otherwise  = error "All numbers must be >= 0 and <= 255"
         where ok  = liftM2 (&&) (<= 255) (>= 0)
               rgb = [r, g, b]

-- temp01's magical function
oOo [] = [];
oOo s  = concat [init s, [toUpper (last s)], tail (reverse s)]

type Peen = String
ben :: Integer -> Peen
ben = ('8' :) . (++ "D") . flip genericReplicate '='

fap = fmap

gf :: Integer -> String
gf n | n <  0    = "NEGATIVE U"
     | n == 0    = "N'T U"
     | n <= 9000 = (unwords . genericReplicate n) "NO" ++ " U"
     | otherwise = "It's over 9000!"

ajpiano = "PANDEMONIUM!!!" :: String

akahn :: Integer -> String
akahn 0 = "akahn :)"
akahn n = "AK" ++ genericReplicate n 'A' ++ "HN!!"

coldhead :: String -> String
coldhead  s | null s    = ">: |"
            | otherwise = "these are truly the last " ++ s

dabear = ("your mom " ++) :: String -> String

dytrivedi :: String -> String
dytrivedi s | null s    = "my wife is happy"
            | otherwise = mappend "my wife is annoyed i spend so much time " s

matjas = interleave "matjas" . enumFrom :: Char -> String

miketaylr :: String -> String
miketaylr s | null ts   = "here, let me open that... wait a minute, there's nothing there, you bitch!"
            | otherwise = unwords ["here, let me open that", s, "for you!"]
            where ts = trim s

nlogax     = (++ "n't")               :: String -> String
paul_irish = ($)                      :: (effin -> rad) -> effin -> rad
rwaldron   = (++ ". Questions?")      :: String -> String
sean       = "koole"                  :: String
seutje     = ("I would have " ++)     :: String -> String
temp01     = Just "awesome"           :: Maybe String
vladikoff  = ("flod " ++) . (++ "!!") :: String -> String

mlu = "much like urself"
muu = "much unlike urself"

trim = trim' . trim'
       where trim' = reverse . dropWhile isSpace

dropInit = take 1 . reverse

relTime t | t <  s     = ["now"]
          | t == s     = ["1 second"]
          | t <  m     = [show t ++ " seconds"]
          | t <  m * 2 = ["1 minute"]            ++ rest m
          | t <  h     = [first m ++ " minutes"] ++ rest m
          | t <  h * 2 = ["1 hour"]              ++ rest h
          | t <  d     = [first h ++ " hours"]   ++ rest h
          | t <  d * 2 = ["1 day"]               ++ rest d
          | t <  w     = [first d ++ " days"]    ++ rest d
          | t <  w * 2 = ["1 week"]              ++ rest w
          | t <  w * 4 = [first w ++ " weeks"]   ++ rest w
          | otherwise  = ["a long time"]
          where first  = show . div t
                rest v | mod t v == 0 = []
                       | otherwise    = relTime $ mod t v
                s = 1; m = s * 60; h = m * 60; d = h * 24; w = d * 7

concatTime [] = []
concatTime xss@(x:_) | x == "now"      = x
                     | 1 == length xss = printf "%s ago." $ concat xss
                     | otherwise       = printf "%s and %s ago."
                                                (intercalate ", " $ init xss)
                                                                  $ last xss

-- Omg postfix function application so you can `car & drive` instead of `drive car`!
infixl 0 &
x & f = f x

ftoc t = 5/9 * (t - 32)
ctof t = (9/5 * t) + 32
