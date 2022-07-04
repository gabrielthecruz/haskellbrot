data Coord = Coord {
    x :: Double,
    y :: Double
} deriving Show

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

-- debug items

xs :: [Double]
xs = [0..800]

ys :: [Double]
ys = [0..600]

coord1 :: Coord 
coord1 = Coord (-0.7469336670838551) (-0.06611018363939891)

coord2 :: Coord 
coord2 = Coord (-0.7469336670838551) (-0.06210350584307189)
