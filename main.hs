import qualified Graphics.Image.Interface as IM
import qualified Graphics.Image as I 
import HSVtoRGB
import Graphics.Image.ColorSpace
import Graphics.Image.Types
import Prelude

sqr :: Double -> Double
sqr x = x ** 2

bulbChecking :: (Double, Double) -> Bool 
bulbChecking (x, y) = (bulbQ * (bulbQ + x - 0.25)) <= (0.25 * sqr y)
    where bulbQ = sqr (x - 0.25) + sqr y

escapeTime :: (Double, Double) -> (Double, Double, Double) -> Int -> (Int, Double, Double)
escapeTime (x, y) (sx, sy, sz) i = 
    if (sqrX + sqrY) > 4 || i >= (round maxIterations)
        then (i, x', y')
        else escapeTime (x, y) (sqrX, sqrY, sqrZ) (succ i)
    where {
        x' = sx - sy + x;
        y' = sz - sx - sy + y;
        sqrX = x' ** 2;
        sqrY = y' ** 2;
        sqrZ = (x' + y') ** 2
    }

smoothColoring :: (Int, Double, Double) -> (Double, Double, Double)
smoothColoring (i, x, y) = if i < (round maxIterations)
    then (fromIntegral (floor (360 * (n / maxIterations))), 1, 1)
    else (fromIntegral (floor (360 * i' / maxIterations)), 1, 0)
    where {
        i' = fromIntegral i;
        n = i' + (1 - log ((log (x*x + y*y) / 2) / (log 2)) / log 2)
    }
    
drawImage :: (Int, Int) -> Pixel RGB Word8
drawImage (y, x) = if bulbChecking (x', y')
    then PixelRGB 0 0 0
    else PixelRGB (round (r rgb)) (round (g rgb)) (round (b rgb))
    where {
        x' = -2.2 + (fromIntegral x) * (3.0 / (fromIntegral width));
        y' = -1.2 + (fromIntegral y) * (2.4 / (fromIntegral height));
        rgb = hsv2rgb (smoothColoring (escapeTime (x', y') (0, 0, 0) 0))
    }

width :: Int
width = 800

height :: Int
height = 600

maxIterations :: Double
maxIterations = 100

main :: IO ()
main = I.writeImageExact PNG [] "haskellbrot.png" (IM.makeImage (height, width) drawImage :: Image VS RGB Word8)
