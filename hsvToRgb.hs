module HSVtoRGB where
import Data.Fixed

data RGB' = RGB' { 
    r :: Double, 
    g :: Double,
    b :: Double
} deriving Show 

isBetween :: Double -> Double -> Double -> Bool
isBetween hue min max = min <= hue && hue < max 

ceil :: Double -> Double 
ceil x = fromIntegral (round x)

hsv2rgb :: (Double, Double, Double) -> RGB' 
hsv2rgb (hue, sat, val)
    | h' <= 1   = RGB' (c') (x') (m')
    | h' <= 2   = RGB' (x') (c') (m')
    | h' <= 3   = RGB' (m') (c') (x')
    | h' <= 4   = RGB' (m') (x') (c')
    | h' <= 5   = RGB' (x') (m') (c')
    | otherwise = RGB' (c') (m') (x')
    where { 
        h' = hue / 60;
        c' = val * 255; 
        m' = (val - sat * val) * 255;
        x' = (((sat * val) * (1 - abs (mod' (hue / 60) 2 - 1))) + (val - val * sat)) * 255
    }