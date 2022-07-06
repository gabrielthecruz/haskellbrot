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
escapeTime (x, y) _ 100 = (100, x, y)
escapeTime (x, y) (sqrX, sqrY, sqrZ) i = if (sqrX + sqrY) > 4
    then (i, x, y)
    else escapeTime (x', y') (sqr x', sqr y', sqr (x' + y')) (i + 1)
    where {
        x' = sqrX - sqrY + x;
        y' = sqrZ - sqrX - sqrY + y
    }

smoothColoring :: (Int, Double, Double) -> (Double, Double, Double)
smoothColoring (i, x, y) = if i < 100
    then (ceil (255 * (n / 100)), 1, 1)
    else (ceil (255 * i' / 100), 1, 0)
    where {
        i' = fromIntegral i;
        n = i' + (1 - log ((log (x*x + y*y) / 2) / (log 2)) / log 2)
    }
    
drawImage :: (Int, Int) -> Pixel RGB Word8
drawImage (y, x) = if bulbChecking (x', y')
    then PixelRGB 0 0 0
    else PixelRGB (round (r rgb)) (round (g rgb)) (round (b rgb))
    where {
        x' = -2.2 + (fromIntegral x) * (3.4 / 800);
        y' = -1.2 + (fromIntegral y) * (2.4 / 600);
        rgb = hsv2rgb (smoothColoring (escapeTime (x', y') (0, 0, 0) 0))
    }

main :: IO ()
main = I.writeImageExact PNG [] "haskellbrot.png" (IM.makeImage (600, 800) drawImage :: Image VS RGB Word8)

p1 = (-2.2 + 1 * (3.4 / 800), -1.2 + 3 * (2.4 / 600))
