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

pendulumModel :: (Fractional a, Floating a, Num a) => a -> (a, a) -> [a] -> a
pendulumModel m (r, pR) [th, pTh] = do
    let g = 40
        pF = transPosToTransMom2 polarToCartesian (r, th)
        (_, y) = polarToCartesian (r, th)
        (pX, pY) = pF (pR, pTh)
    (pX * pX + pY * pY) / (2 * m) + m * g * y

pendulumView :: (Float, Float) -> [Float] -> Picture
pendulumView (r, _) [th, _] = do
    let (x, y) = polarToCartesian (r, th)
    pictures [line [(0, 0), (x, y)], Translate x y (thickCircle 3 5)]

main :: IO ()
main = do
    simulate window white 128 [-pi/3, 0] (pendulumView (150, 0)) (\_ -> solveHamilEq (pendulumModel 1 (150, 0)) euler)
