module Main (main) where

import Graphics.Gloss
import Hamilton
import Ode

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

free :: (Fractional a, Floating a, Num a) => [a] -> a
free [_, pX, _, pY] = (pX * pX + pY * pY) / (2 * m)
    where m = 1

point :: [Float] -> Picture
point [x, _, y, _] = translate x y (circle 5)

draw :: [Float] -> Picture
draw [th, pTh] = do
    let r = 150
    let (x, y) = (r * cos th, r * sin th)
    pictures [line [(0, 0), (x, y)], Translate x y (thickCircle 3 5)]

f :: (Fractional a, Floating a, Num a) => (a, a) -> (a, a)
f (r, th) = (r * cos th, r * sin th)

pendulum :: (Fractional a, Floating a, Num a) => [a] -> a
pendulum [th, pTh] = do
    let r = 150
        pR = 0
    let g = 40
        m = 1
        pF = transPosToTransMom2 f (r, th)
        (_, y) = f (r, th)
        (pX, pY) = pF (pR, pTh)
    (pX * pX + pY * pY) / (2 * m) + m * g * y

main :: IO ()
main = do
    -- simulate window white 24 [0, 100, 0, 50] point (\_ -> euler free)
    simulate window white 24 [-pi/3, 0] draw (\_ -> euler pendulum)
