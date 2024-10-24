{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Hamilton where

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse as Rev
import Numeric.AD.Internal.Reverse
import Type.Reflection

hamilEq :: Num a => (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s a] -> Rev.Reverse s a) -> [a] -> [a]
hamilEq hamil state = do
    let dHd_ = grad hamil state -- dH / dq, dH / dp
    let d_dt = calc dHd_ where
        calc [] = []
        calc (dHdq : dHdp : xs) = dHdp : -dHdq : calc xs -- dq / dt = dH / dp, dp / dt = -dH / dq
    d_dt

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
        (p1', p2') = (p1 * b11 + p2 * b21, p1 * b12 + p2 * b22) -- (p1', p2') = (p1, p2) J^-1
    (p1', p2')