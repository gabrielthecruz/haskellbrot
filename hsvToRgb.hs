module HSVtoRGB where

import Prelude
import Data.Fixed

data CustomHSV = CustomHSV {
    hue :: Int,
    sat :: Double,
    val :: Double
} deriving Show 

data RGB = RGB Int Int Int deriving Show

c :: CustomHSV -> Double 
c (CustomHSV _ s v) = s * v

x :: CustomHSV -> Double 
x hsv = (c hsv) * fromIntegral(1 - abs (mod' (div (hue hsv) 60) 2 - 1))

m :: CustomHSV -> Double 
m hsv = val hsv - c hsv

convertColor :: Double -> CustomHSV -> Double
convertColor x' hsv = (x' + m hsv) * 255

isBetween :: Int -> Int -> Int -> Bool
isBetween min max n = min <= n && n < max 

red :: CustomHSV -> Double 
red (CustomHSV h s v) 
    | isBetween 0 60 h || isBetween 300 360 h = convertColor (c hsv) hsv
    | isBetween 60 120 h || isBetween 240 300 h = convertColor (x hsv) hsv
    | otherwise = convertColor 0 hsv
    where hsv = CustomHSV h s v

green :: CustomHSV -> Double 
green (CustomHSV h s v) 
    | isBetween 0 60 h || isBetween 180 240 h = convertColor (x hsv) hsv
    | isBetween 60 180 h = convertColor (c hsv) hsv
    | otherwise = convertColor 0 hsv
    where hsv = CustomHSV h s v

blue :: CustomHSV -> Double 
blue (CustomHSV h s v) 
    | isBetween 120 180 h || isBetween 300 360 h = convertColor (x hsv) hsv
    | isBetween 180 300 h = convertColor (c hsv) hsv
    | otherwise = convertColor 0 hsv
    where hsv = CustomHSV h s v

hsv2rgb :: CustomHSV -> RGB 
hsv2rgb hsv = RGB (round (red hsv)) (round (green hsv)) (round (blue hsv))
