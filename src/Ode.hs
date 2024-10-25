module Ode where

instance Num a => Num [a] where
    (+) = zipWith (+)
    (-) = zipWith (-)
    abs = map abs

-- solve ds/dt = f(s)

-- s_n+1 = s_n + f(s_n) h
rk1 :: Num a => ([a] -> [a]) -> a -> [a] -> [a]
rk1 f dt state = state + map (* dt) (f state)

euler :: Num a => ([a] -> [a]) -> a -> [a] -> [a]
euler = rk1

-- s_n+1 = s_n + { (1-1/2a) f(s_n) + 1/2a f(s_n + af(s_n)h) } h
rk2 :: Float -> ([Float] -> [Float]) -> Float -> [Float] -> [Float] -- Floating a => a -> ([a] -> [a]) -> a -> [a] -> [a]
rk2 a f dt s = do
    let (.*) x ys = map (x *) ys
    let (*.) xs y = map (* y) xs
    let fs = f s
    s + ( ((1-1/(2*a)) .* fs) + ((1/(2*a)) .* f (s + (a .* fs *. dt))) ) *. dt

midpoint :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
midpoint = rk2 0.5

heun :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
heun = rk2 1

-- s_n+1 = s_n + (k_1 + 2k_2 + 2k_3 + k_4) h/6
-- k_1 = f(s_n)
-- k_2 = f(s_n + k_1 h/2)
-- k_3 = f(s_n + k_2 h/2)
-- k_4 = f(s_n + k_3 h)
rk4Classical :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
rk4Classical f dt s = s + (k1 + 2 .* k2 + 2 .* k3 + k4) *. (dt / 6) where
    (.*) x ys = map (x *) ys
    (*.) xs y = map (* y) xs

    k1 = f s
    k2 = f $ s + (k1 *. (dt/2))
    k3 = f $ s + (k2 *. (dt/2))
    k4 = f $ s + (k3 *. dt)

loss :: [Float] -> [Float] -> Float
loss a b = minimum $ abs (a - b)

-- s_n+1 = s_n + f((s_n + s_n+1) / 2) h
midpointImplicit :: ([Float] -> [Float]) -> Float -> [Float] -> [Float]
midpointImplicit f dt s = iter s where
    (.*) x ys = map (x *) ys
    (*.) xs y = map (* y) xs
    iter :: [Float] -> [Float]
    iter sNext = do
        let sNext' = s + f ((s + sNext) *. 0.5) *. dt
        if loss sNext sNext' <= 1e-6 then sNext' else iter sNext'
