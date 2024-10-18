module Node where

import qualified Data.Map.Strict as M

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

type State = M.Map String Float

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

eval :: State -> Node -> Float
eval _ (Num v) = v
eval state (Var s) = state M.! s
eval state (Add x y) = eval state x + eval state y
eval state (Sub x y) = eval state x - eval state y
eval state (Mul x y) = eval state x * eval state y
eval state (Div x y) = eval state x / eval state y
eval state (Pow x n) = (eval state x) ^ n
eval state (Neg x) = - eval state x
eval state (Exp x) = exp (eval state x)
eval state (Log x) = log (eval state x)
eval state (Sin x) = sin (eval state x)
eval state (Cos x) = cos (eval state x)
eval state (Tan x) = tan (eval state x)

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
