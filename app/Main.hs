module Main (main) where

import qualified Data.Map.Strict as M
import Graphics.Gloss

import Node
import Ode

window :: Display
window = InWindow "Pendulum" (640, 480) (100, 100)

draw :: (Node, Node) -> State -> Picture
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

    -- initial state
    let initState = M.fromList [("r", 150), ("th", -pi / 3), ("p", 0), ("l", 0)]
    let pdv = M.fromList [("r", Num 0), ("th", dotth), ("p", dotp), ("l", dotl)]

    simulate window white 24 initState (draw (x, y)) (\_ -> eulerMethod pdv)
