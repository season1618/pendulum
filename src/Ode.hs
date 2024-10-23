{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Ode where

import Data.Reflection
import Numeric.AD
import Numeric.AD.Mode.Reverse as Rev
import Numeric.AD.Internal.Reverse
import Type.Reflection

import Hamilton

euler :: (forall s. (Reifies s Tape, Typeable s) => [Rev.Reverse s Float] -> Rev.Reverse s Float) -> Float -> [Float] -> [Float]
euler hamil dt state = do
    let dstate = hamilEq hamil state
    zipWith (\s u -> s + u * dt) state dstate
