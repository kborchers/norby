{-# LANGUAGE NoMonomorphismRestriction,
             ScopedTypeVariables,
             UnicodeSyntax #-}

module L where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Char
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Numeric
import           System
import           System.IO
import           System.Process
import           System.Random
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
ben :: Int -> Peen
ben = ('8' :) . (++ "D") . flip replicate '='

gf n | n <  0    = "NEGATIVE U"
     | n == 0    = "N'T U"
     | n <= 9000 = (unwords . replicate n) "NO" ++ " U"
     | otherwise = "It's over 9000!"

ajpiano = "PANDEMONIUM!!!"

akahn s | last s == '?' = "did you mean " ++ s
        | otherwise     = "that's not "   ++ s

coldhead  s | null s    = ">: |"
            | otherwise = "these are truly the last " ++ s

dabear = ("your mom " ++)

dytrivedi s | null s    = "my wife is happy"
            | otherwise = mappend "my wife is annoyed i spend so much time " s

matjas = interleave "matjas" . enumFrom

miketaylr s | null ts   = "here, let me open that... wait a minute, there's nothing there, you bitch!"
            | otherwise = unwords ["here, let me open that", s, "for you!"]
            where ts = trim s

nlogax     = (++ "n't")
paul_irish = "ryan_irelan"
sean       = "koole"
seutje     = ("I would of " ++)
temp01     = Just "awesome"
vladikoff  = ("flod " ++) . (++ "!!")

mlu = "much like urself"
muu = "much unlike urself"

trim = trim' . trim'
       where trim' = reverse . dropWhile isSpace

-- Omg postfix function application so you can `car drive` instead of `drive car`!
infixl 0 &
x & f = f x
