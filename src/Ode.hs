{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Ode where

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse as Rev
import Numeric.AD.Internal.Reverse
import Type.Reflection

euler :: (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s Float] -> Rev.Reverse s Float) -> Float -> [Float] -> [Float]
euler hamil dt state = do
    let [dHdq1, dHdp1, dHdq2, dHdp2] = grad hamil state -- dH / dq, dH / dp
    let dstate = [dHdp1, -dHdq1, dHdp2, -dHdq2] -- dq / dt = dH / dp, dp / dt = -dH / dq
    zipWith (\s u -> s + u * dt) state dstate
