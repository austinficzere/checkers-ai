module Main where

import Moves
import ApplyMove
import ABSearch
import Heuristic
import Checkers.Types
import Checkers.BasicPrint
import Checkers.FrontEnd.Types
import Checkers.FrontEnd.Basic


gameEngine :: CheckersEngine
gameEngine m g = apply_move m g

main :: IO()
main = frontend $ GameConfig { engine = gameEngine,
                               blackMove = Ai black_ai,
                               redMove = Ai red_ai,
                               state = initialGameState}