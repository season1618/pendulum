module Main (main) where

import Graphics.Gloss
import Hamilton
import Ode

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

free :: Fractional a => [a] -> a
free [_, pX, _, pY] = (pX * pX + pY * pY) / (2 * m)
    where m = 1

point :: [Float] -> Picture
point [x, _, y, _] = translate x y (circle 5)

harmOscModel :: Floating a => (a, a) -> [a] -> a
harmOscModel (m, k) [x, p] = p * p / (2 * m) + k * x * x / 2

harmOscKinetic :: Floating a => a -> [a] -> a
harmOscKinetic m [p] = p * p / (2 * m)

harmOscPotential :: Floating a => a -> [a] -> a
harmOscPotential k [x] = k * x * x / 2

harmOscView :: [Float] -> Picture
harmOscView [x, p] = pictures [circle 150, translate x p (circleSolid 5)]

pendulumModel :: Floating a => a -> (a, a) -> [a] -> a
pendulumModel m (r, pR) [th, pTh] = do
    let g = 200
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
    simulate (InWindow "Harmonic Oscillator" (640, 480) (100, 100)) white 24 [150, 0] harmOscView (\_ -> solveHamilSum si4 (harmOscKinetic 1) (harmOscPotential 1))
    simulate (InWindow "Harmonic Oscillator" (640, 480) (100, 100)) white 24 [150, 0] harmOscView (\_ -> solveHamilEq (harmOscModel (1, 1)) midpointImplicit)
    simulate window white 128 [-pi/3, 0] (pendulumView (150, 0)) (\_ -> solveHamilEq (pendulumModel 1 (150, 0)) midpointImplicit)
