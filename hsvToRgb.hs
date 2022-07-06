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

hsv2rgb :: (Double, Double, Double) -> RGB' -- (Double, Double, Double)
hsv2rgb (hue, sat, val)
    | h' == 1   = RGB' (ceil c') (ceil x') (ceil m')
    | h' == 2   = RGB' (ceil x') (ceil c') (ceil m')
    | h' == 3   = RGB' (ceil m') (ceil c') (ceil x')
    | h' == 4   = RGB' (ceil m') (ceil x') (ceil c')
    | h' == 5   = RGB' (ceil x') (ceil m') (ceil c')
    | otherwise = RGB' (ceil c') (ceil m') (ceil x')
    where { 
        h' = hue / 60;
        c' = val * 255; 
        m' = (val - sat * val) * 255;
        x' = (((sat * val) * (1 - abs (mod' (hue / 60) 2 - 1))) + (val - val * sat)) * 255
    }