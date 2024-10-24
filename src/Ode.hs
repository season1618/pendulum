module Ode where

-- solve ds/dt = f(s)

-- s_n+1 = s_n + f(s_n) h
euler :: Num a => ([a] -> [a]) -> a -> [a] -> [a]
euler f dt state = do
    let dstate = f state
    zipWith (\s u -> s + u * dt) state dstate

-- s_n+1 = s_n + { (1-1/2a) f(s_n) + 1/2a f(s_n + af(s_n)h) } h
rk2 :: Float -> ([Float] -> [Float]) -> Float -> [Float] -> [Float] -- Floating a => a -> ([a] -> [a]) -> a -> [a] -> [a]
rk2 a f dt s = do
    let (.+.) = zipWith (+)
    let (.*) x ys = map (x *) ys
    let (*.) xs y = map (* y) xs
    let fs = f s
    s .+. ( ( ((1-1/(2*a)) .* fs) .+. ((1/(2*a)) .* f (s .+. (a .* fs *. dt))) ) *. dt)

midpoint :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
midpoint = rk2 0.5

heun :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
heun = rk2 1
