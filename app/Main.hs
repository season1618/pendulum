module Main (main) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

data Node = Num Float
          | Var String
          | Add Node Node
          | Sub Node Node
          | Mul Node Node
          | Div Node Node
          | Pow Node Int
          | Neg Node
          | Exp Node
          | Log Node
          | Sin Node
          | Cos Node
          | Tan Node

instance Eq Node where
    (Num x) == (Num y) = x == y
    (Var x) == Var y = x == y
    Add x1 x2 == Add y1 y2 = (x1 == y1) && (x2 == y2)
    Sub x1 x2 == Sub y1 y2 = (x1 == y1) && (x2 == y2)
    Mul x1 x2 == Mul y1 y2 = (x1 == y1) && (x2 == y2)
    Div x1 x2 == Div y1 y2 = (x1 == y1) && (x2 == y2)
    Pow x1 x2 == Pow y1 y2 = (x1 == y1) && (x2 == y2)
    Neg x == Neg y = x == y
    Exp x == Exp y = x == y
    Log x == Log y = x == y
    Sin x == Sin y = x == y
    Cos x == Cos y = x == y
    Tan x == Tan y = x == y
    _ == _ = False

instance Num Node where
    x + y = Add x y
    x - y = Sub x y
    x * y = Mul x y
    negate x = Neg x
    fromInteger x = Num (fromIntegral x)

instance Fractional Node where
    x / y = Div x y

instance Show Node where
    show node =
        case reduce node of
            Num v -> show v
            Var s -> s
            Add x y -> "(" ++ show x ++ " + " ++ show y ++ ")"
            Sub x y -> "(" ++ show x ++ " - " ++ show y ++ ")"
            Mul x y -> "(" ++ show x ++ " * " ++ show y ++ ")"
            Div x y -> "(" ++ show x ++ " / " ++ show y ++ ")"
            Pow x n -> "(" ++ show x ++ " ^ " ++ show n ++ ")"
            Neg x -> "- " ++ show x
            Exp x -> "exp " ++ show x
            Log x -> "log " ++ show x
            Sin x -> "sin " ++ show x
            Cos x -> "cos " ++ show x
            Tan x -> "tan " ++ show x

instance Floating Node where
    sin x = Sin x
    cos x = Cos x
    tan x = Tan x

(/\) :: Node -> Int -> Node
(/\) x y = Pow x y

reduce :: Node -> Node
reduce (Add x y) | x == Num 0.0 = reduce y
                 | y == Num 0.0 = reduce x
                 | otherwise = Add (reduce x) (reduce y)
reduce (Sub x y) | x == Num 0.0 = Neg (reduce y)
                 | y == Num 0.0 = reduce x
                 | otherwise = Sub (reduce x) (reduce y)
reduce (Mul x y) | x == Num 0.0 = Num 0.0
                 | y == Num 0.0 = Num 0.0
                 | x == Num 1.0 = reduce y
                 | y == Num 1.0 = reduce x
                 | otherwise = Mul (reduce x) (reduce y)
reduce (Div x y) | x == Num 0.0 = Num 0.0
                 | y == Num 1.0 = reduce x
                 | otherwise = Div (reduce x) (reduce y)
reduce (Pow x n) | n == 0 = Num 1.0
                 | n == 1 = reduce x
                 | otherwise = Pow (reduce x) n
reduce (Neg x)   | x == Num 0.0 = Num 0.0
                 | otherwise = Neg (reduce x)
reduce (Exp x) = Exp (reduce x)
reduce (Log x) = Log (reduce x)
reduce (Sin x) = Sin (reduce x)
reduce (Cos x) = Cos (reduce x)
reduce (Tan x) = Tan (reduce x)
reduce x = x

eval :: Map.Map String Float -> Node -> Float
eval _ (Num v) = v
eval val (Var s) = val Map.! s
eval val (Add x y) = eval val x + eval val y
eval val (Sub x y) = eval val x - eval val y
eval val (Mul x y) = eval val x * eval val y
eval val (Div x y) = eval val x / eval val y
eval val (Pow x n) = (eval val x) ^ n
eval val (Neg x) = - eval val x
eval val (Exp x) = exp (eval val x)
eval val (Log x) = log (eval val x)
eval val (Sin x) = sin (eval val x)
eval val (Cos x) = cos (eval val x)
eval val (Tan x) = tan (eval val x)

diff :: Node -> String -> Node
diff (Num _) _ = Num 0
diff (Var s) t = if s == t then 1 else 0
diff (Add x y) t = (diff x t) + (diff y t)
diff (Sub x y) t = (diff x t) - (diff y t)
diff (Mul x y) t = (diff x t) * y + x * (diff y t)
diff (Div x y) t = ((diff x t) * y - x * (diff y t)) / (y /\ 2)
diff (Pow x n) t = Num (fromIntegral n) * Pow x (n-1) * diff x t
diff (Neg x) t = Neg (diff x t)
diff (Exp x) t = Exp x * diff x t
diff (Log x) t = diff x t / Log x
diff (Sin x) t = diff x t * Cos x
diff (Cos x) t = Neg (diff x t * Sin x)
diff (Tan x) t = diff x t / (Cos x /\ 2)

type Model = Map.Map String Float

eulerMethod :: (Map.Map String Node) -> Float -> Model -> Model
eulerMethod pdvs dt state = Map.mapWithKey (\name -> \u -> u + eval state (pdvs Map.! name) * dt) state

draw :: (Node, Node) -> Model -> Picture
draw (x, y) state = do
    let xValue = eval state x
        yValue = eval state y
    pictures [line [(0, 0), (xValue, yValue)], Translate xValue yValue (thickCircle 3 5)]

main :: IO ()
main = do
    -- constant
    let g = Num 40
        m = Num 1.0

    -- generalized coodinate and momentum
    let r = Var "r"
        th = Var "th"
        p = Var "p"
        l = Var "l"

    -- coodinate transformation
    let x = r * cos th
        y = r * sin th

    let a11 = diff x "r"
        a12 = diff x "th"
        a21 = diff y "r"
        a22 = diff y "th"

    let det = a11 * a22 - a12 * a21

    let b11 = a22 / det
        b12 = -a12 / det
        b21 = -a21 / det
        b22 = a11 / det

    let px = p * b11 + l * b21
        py = p * b12 + l * b22

    -- Hamiltonian in Cartesian coodinate
    let h = (px /\ 2 + py /\ 2) / (2 * m) + m * g * y

    -- Euler method
    let dotr = diff h "p"
        dotth = diff h "l"
        dotp = - diff h "r"
        dotl = - diff h "th"

    -- initial model
    let initModel = Map.fromList [("r", 150), ("th", -pi / 3), ("p", 0), ("l", 0)]
    let pdv = Map.fromList [("r", dotr), ("th", dotth), ("p", dotp), ("l", dotl)]

    simulate window white 24 initModel (draw (x, y)) (\_ -> eulerMethod pdv)
