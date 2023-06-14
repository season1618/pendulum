module Main (main) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Control.Concurrent
-- import Lib

window :: Display
window = InWindow "Kepler Problem" (640, 480) (100, 100)

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
    x == y = False

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

eulerMethod :: (Map.Map String Float) -> (Node, Node, Node, Node) -> (Node, Node) -> IO ()
eulerMethod state (dq1, dq2, dp1, dp2) (x, y) = do
    let q1 = state Map.! "q1"
    let q2 = state Map.! "q2"
    let p1 = state Map.! "p1"
    let p2 = state Map.! "p2"

    let q1Next = q1 + eval state dq1
    let q2Next = q2 + eval state dq2
    let p1Next = p1 + eval state dp1
    let p2Next = p2 + eval state dp2

    let stateNext = Map.insert "q1" q1Next . Map.insert "q2" q2Next . Map.insert "p1" p1Next . Map.insert "p2" p2Next $ state

    display window white $ Translate (eval state x) (eval state y) (Circle 3)
    print (eval state x, eval state y)
    threadDelay (1000)

    eulerMethod stateNext (dq1, dq2, dp1, dp2) (x, y)

main :: IO ()
main = do
    -- constant
    let g = Num 9.8
    let m = Num 1.0

    -- generalized coodinate and momentum
    let q1 = Var "q1"
    let q2 = Var "q2"
    let p1 = Var "p1"
    let p2 = Var "p2"

    -- coodinate transformation
    let x = q1 * cos q2
    let y = q1 * sin q2

    let a11 = diff x "q1"
    let a12 = diff x "q2"
    let a21 = diff y "q1"
    let a22 = diff y "q2"

    let det = a11 * a22 - a12 * a21

    let b11 = a22 / det
    let b12 = -a12 / det
    let b21 = -a21 / det
    let b22 = a11 / det

    let px = p1 * b11 + p2 * b21
    let py = p1 * b12 + p2 * b22

    -- Hamiltonian in Cartesian coodinate
    let h = (px /\ 2 + py /\ 2) / (2 * m) + m * g * y

    -- Euler method
    let dt = Num 0.1
    let dq1 = diff h "p1" * dt
    let dq2 = diff h "p2" * dt
    let dp1 = - diff h "q1" * dt
    let dp2 = - diff h "q2" * dt

    putStrLn $ show h
    eulerMethod (Map.fromList [("q1", 1), ("q2", 0), ("p1", 0), ("p2", 0)]) (dq1, dq2, dp1, dp2) (x, y)
