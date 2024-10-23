module Main (main) where

import Graphics.Gloss
import Hamilton
import Ode

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

draw :: [Float] -> Picture
draw [r, pR, th, pTh] = do
    let (x, y) = (r * cos th, r * sin th)
    pictures [line [(0, 0), (x, y)], Translate x y (thickCircle 3 5)]

f :: (Fractional a, Floating a, Num a) => (a, a) -> (a, a)
f (r, th) = (r * cos th, r * sin th)

pendulum :: (Fractional a, Floating a, Num a) => [a] -> a
pendulum [r, pR, th, pTh] = do
    let g = 40
        m = 1
        pF = transPosToTransMom2 f (r, th)
        (_, y) = f (r, th)
        (pX, pY) = pF (pR, pTh)
    (pX * pX + pY * pY) / (2 * m) + m * g * y

main :: IO ()
main = do
    simulate window white 24 [150, 0, -pi/3, 0] draw (\_ -> euler pendulum)
