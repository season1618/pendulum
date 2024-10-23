{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import qualified Data.Map.Strict as M
import Data.Reflection
import Graphics.Gloss
import Numeric.AD
import Numeric.AD.Mode.Reverse as Rev
import Numeric.AD.Internal.Reverse
import Type.Reflection

import Node
import Ode

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

draw :: [Float] -> Picture
draw [r, pR, th, pTh] = do
    let (x, y) = (r * cos th, r * sin th)
    pictures [line [(0, 0), (x, y)], Translate x y (thickCircle 3 5)]

jacob2 :: Num a => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> ((a, a), (a, a))
jacob2 f (x1, x2) = do
    let [[a11, a12], [a21, a22]] = jacobian (\[s1, s2] -> let (t1, t2) = f (s1, s2) in [t1, t2]) [x1, x2]
    ((a11, a12), (a21, a22))

jacobInv2 :: (Fractional a, Num a) => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> ((a, a), (a, a))
jacobInv2 f (x1, x2) = do
    let ((a11, a12), (a21, a22)) = jacob2 f (x1, x2)
        det = a11 * a22 - a12 * a21
        ((b11, b12), (b21, b22)) = ((a22 / det, -a12 / det), (-a21 / det, a11 / det))
    ((b11, b12), (b21, b22))

transPosToTransMom2 :: (Fractional a, Num a) => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> (a, a) -> (a, a)
transPosToTransMom2 f (x1, x2) (p1, p2) = do
    let ((b11, b12), (b21, b22)) = jacobInv2 f (x1, x2)
        (p1', p2') = (b11 * p1 + b12 * p2, b21 * p1 + b22 * p2)
    (p1', p2')

f :: (Fractional a, Floating a, Num a) => (a, a) -> (a, a)
f (r, th) = (r * cos th, r * sin th)

pendulum :: (Fractional a, Floating a, Num a) => [a] -> a
pendulum [r, pR, th, pTh] = do
    let g = 40
        m = 1
        pF = transPosToTransMom2 f (r, th)
        (x, y) = f (r, th)
        (pX, pY) = pF (pR, pTh)
    (pX * pX + pY * pY) / (2 * m) + m * g * y

solve :: (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s Float] -> Rev.Reverse s Float) -> Float -> [Float] -> [Float]
solve hamil dt state = do
    let [dHdq1, dHdp1, dHdq2, dHdp2] = grad hamil state -- dH / dq, dH / dp
    let dstate = [dHdp1, -dHdq1, dHdp2, -dHdq2] -- dq / dt = dH / dp, dp / dt = -dH / dq
    zipWith (\s u -> s + u * dt) state dstate

main :: IO ()
main = do
    simulate window white 24 [150, 0, -pi/3, 0] draw (\_ -> solve pendulum)
