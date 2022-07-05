import Prelude
import Graphics.Image as I
import qualified HSVtoRGB as HTR

data Coord = Coord {
    x :: Double,
    y :: Double,
    color :: HTR.CustomHSV
} deriving Show

data Axis = AxisX | AxisY 

sqr :: Double -> Double
sqr x = x ** 2

bulbX :: Double -> Double
bulbX x = x - 0.25

bulbQ :: Double -> Double -> Double 
bulbQ x y = sqr (x - 0.25) + sqr y

bulbChecking :: Coord -> Bool 
bulbChecking (Coord x y _) = 
    if bulbQ x y * ((bulbQ x y) + bulbX x) <= 0.25 * sqr y
        then True 
        else False

coordinates :: [Double] -> [Double] -> [Coord]
coordinates xs ys = [(Coord x y (HTR.CustomHSV 0 1 0)) | x <- xs, y <- ys]

distance :: Double -> Double -> Double -> Double
distance min max n = (abs min + abs max) / n 

parseAxis :: Double -> Axis -> Double 
parseAxis 0 AxisX = -2.2
parseAxis n AxisX = n * (distance (-2.2) 0.8 800)
parseAxis 0 AxisY = -1.2
parseAxis n AxisY = n * (distance (-1.2) 1.2 600)

pixelToCoord :: Coord -> Coord
pixelToCoord (Coord x y _) = Coord (parseAxis x AxisX) (parseAxis y AxisY) (HTR.CustomHSV 0 0 0)

escapeTimeX :: Double -> Double -> Double 
escapeTimeX x y = sqr x - sqr y + x 

escapeTimeY :: Double -> Double -> Double 
escapeTimeY x y = sqr (x + y) - sqr x - sqr y + y

escapeTimeCheck :: Double -> Double -> Bool 
escapeTimeCheck x y = if sqr x + sqr y > 4 then True else False

escapeTime :: Double -> Coord -> (Double, Double, Double)
escapeTime 100 (Coord x y _) = (100, x, y)
escapeTime i (Coord x y _) = if escapeTimeCheck x y
    then (i, x, y)
    else escapeTime (i+1) (Coord (escapeTimeX x y) (escapeTimeY x y) (HTR.CustomHSV 0 0 0))

smoothColoringN :: Double -> Double -> Double -> Double 
smoothColoringN x y i = i + (1 - log (log (sqr x + sqr y) / log 2)) / log 2

smoothColoring :: Double -> Double -> Double -> HTR.CustomHSV
smoothColoring i x y = if i < 100
    then HTR.CustomHSV (round (360 * (smoothColoringN x y i) / 100)) 1 1
    else HTR.CustomHSV (round (360 * i / 100)) 1 0

applyColor :: (Double, Double, Double) -> Coord -> Coord
applyColor (i, x1, y1) coord = Coord (x coord) (y coord) (smoothColoring i x1 y1)



-- debug
xs :: [Double]
xs = [0..800]

ys :: [Double]
ys = [0..600]

pixel1 :: Coord
pixel1 = head (coordinates xs ys)

pixel2 :: Coord
pixel2 = head (tail (coordinates xs ys))

coord1 :: Coord 
coord1 = pixelToCoord pixel1

coord2 :: Coord 
coord2 = pixelToCoord pixel2 
