module Ode where

import qualified Data.Map.Strict as M
import Node

eulerMethod :: (M.Map String Node) -> Float -> State -> State
eulerMethod pdvs dt state = M.mapWithKey (\name -> \u -> u + eval state (pdvs M.! name) * dt) state
