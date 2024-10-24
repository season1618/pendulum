module Ode where

-- solve ds/dt = f(s)

-- s_n+1 = s_n + f(s_n) * h
euler :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
euler f dt state = do
    let dstate = f state
    zipWith (\s u -> s + u * dt) state dstate
