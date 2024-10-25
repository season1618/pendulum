{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Hamilton where

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse as Rev
import Numeric.AD.Internal.Reverse
import Type.Reflection

instance Num a => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)

hamilEq :: Num a => (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s a] -> Rev.Reverse s a) -> [a] -> [a]
hamilEq hamil state = do
    let dHd_ = grad hamil state -- dH / dq, dH / dp
    let d_dt = calc dHd_ where
        calc [] = []
        calc (dHdq : dHdp : xs) = dHdp : -dHdq : calc xs -- dq / dt = dH / dp, dp / dt = -dH / dq
    d_dt

solveHamilEq :: Num a
    => (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s a] -> Rev.Reverse s a)
    -> (([a] -> [a]) -> a -> [a] -> [a])
    -> a -> [a] -> [a]
solveHamilEq hamil method = method (hamilEq hamil)

-- H(p, q) = H_1(p) + H_2(q)
solveHamilSum :: Num a
    => (a -> ([a] -> [a], [a] -> [a]) -> ([a], [a]) -> ([a], [a]))
    -> (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s a] -> Rev.Reverse s a)
    -> (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s a] -> Rev.Reverse s a)
    -> a -> [a] -> [a]
solveHamilSum method hamilP hamilQ dt state = do
    let (q, p) = unzip' state
        dqdt = grad hamilP
        dpdt = map negate . grad hamilQ
        (q', p') = method dt (dqdt, dpdt) (q, p)
    zip' (q', p')
    where
        zip' :: ([a], [a]) -> [a]
        zip' ([], []) = []
        zip' (x:xs, y:ys) = x : y : zip' (xs, ys)
        unzip' :: [a] -> ([a], [a])
        unzip' [] = ([], [])
        unzip' (x : y : xys) = let (xs, ys) = unzip' xys in (x:xs, y:ys)

si1 :: Num a => a -> ([a] -> [a], [a] -> [a]) -> ([a], [a]) -> ([a], [a])
si1 dt (dqdt, dpdt) (q, p) = (q', p') where
    q' = q + map (* dt) (dqdt p)  -- q_n+1 = q_n + dq/dt(p_n  ) h
    p' = p + map (* dt) (dpdt q') -- p_n+1 = p_n + dp/dt(q_n+1) h

polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, th) = (r * cos th, r * sin th)

jacob2 :: Num a => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> ((a, a), (a, a))
jacob2 f (x1, x2) = do
    let [[a11, a12], [a21, a22]] = jacobian (\[s1, s2] -> let (t1, t2) = f (s1, s2) in [t1, t2]) [x1, x2]
    ((a11, a12), (a21, a22))

jacobInv2 :: Fractional a => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> ((a, a), (a, a))
jacobInv2 f (x1, x2) = do
    let ((a11, a12), (a21, a22)) = jacob2 f (x1, x2)
        det = a11 * a22 - a12 * a21
        ((b11, b12), (b21, b22)) = ((a22 / det, -a12 / det), (-a21 / det, a11 / det))
    ((b11, b12), (b21, b22))

transPosToTransMom2 :: Fractional a => (forall s. (Reifies s Tape, Typeable s) => (Rev.Reverse s a, Rev.Reverse s a) -> (Rev.Reverse s a, Rev.Reverse s a)) -> (a, a) -> (a, a) -> (a, a)
transPosToTransMom2 f (x1, x2) (p1, p2) = do
    let ((b11, b12), (b21, b22)) = jacobInv2 f (x1, x2)
        (p1', p2') = (p1 * b11 + p2 * b21, p1 * b12 + p2 * b22) -- (p1', p2') = (p1, p2) J^-1
    (p1', p2')