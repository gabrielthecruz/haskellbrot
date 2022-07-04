import Prelude

data Color = Color {
    hue :: Integer,
    value :: Integer
} deriving Show

data Coord = Coord {
    x :: Double,
    y :: Double,
    color :: Color
} deriving Show

data Axis = X | Y 

sqr :: Double -> Double
sqr x = x ** 2

bulbX :: Double -> Double
bulbX x = x - 0.25

bulbQ :: Double -> Double -> Double 
bulbQ x y = sqr (x - 0.25) + sqr y

bulbChecking :: Coord -> Bool 
bulbChecking (Coord x y) = 
    if bulbQ x y * ((bulbQ x y) + bulbX x) <= 0.25 * sqr y
        then True 
        else False

coordinates :: [Double] -> [Double] -> [Coord]
coordinates xs ys = [(Coord x y) | x <- xs, y <- ys]

distance :: Double -> Double -> Double -> Double
distance min max n = (abs min + abs max) / n 

parseAxis :: Double -> Axis -> Double 
parseAxis 0 X = -2.2
parseAxis n X = n * (distance (-2.2) 0.8 800)
parseAxis 0 Y = -1.2
parseAxis n Y = n * (distance (-1.2) 1.2 600)

pixelToCoord :: Coord -> Coord
pixelToCoord (Coord x y) = Coord (parseAxis x X) (parseAxis y Y)


escapeTimeX :: Double -> Double -> Double 
escapeTimeX x y = sqr x - sqr y + x 

escapeTimeY :: Double -> Double -> Double 
escapeTimeY x y = sqr (x + y) - sqr x - sqr y + y

escapeTimeCheck :: Double -> Double -> Bool 
escapeTimeCheck x y = if sqr x + sqr y > 4 then True else False

escapeTime :: Integer -> Coord -> (Integer, Double, Double)
escapeTime 100 (Coord x y) = (100, x, y)
escapeTime i (Coord x y) = if escapeTimeCheck x y
    then (i, x, y)
    else escapeTime (i+1) (Coord (escapeTimeX x y) ((escapeTimeY x y)))

-- debug items
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
-- coord1 = Coord (-0.7469336670838551) (-0.06611018363939891)

coord2 :: Coord 
coord2 = pixelToCoord pixel2 
-- coord2 = Coord (-0.7469336670838551) (-0.06210350584307189)
