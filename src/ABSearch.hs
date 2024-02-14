module ABSearch where

import Moves
import ApplyMove
import Heuristic
import Checkers.Types

-- NOTE: The structure of the a-b pruning and the minimax functions came from Niran Pon's example code that he posted for Tic Tac Toe.

-- This is our AI for the red pieces, we call our minimax function with a ply of 5 and take the snd of the return value, which is the move that
-- the AI decided to make. Since it is a maybe value, we remove the Just and return only the move. 
red_ai :: GameState -> Move
red_ai g = let
        (Just move) = snd $ minimax g 5
    in move

-- This is our AI for the black pieces, we call our minimax function with a ply of 5 and take the snd of the return value, which is the move that
-- the AI decided to make. Since it is a maybe value, we remove the Just and return only the move. 
black_ai :: GameState -> Move
black_ai g = let
        (Just move) = snd $ minimax g 5
    in move

-- Used as our top and bottom values for the minimax search.
infinity :: Float
infinity = 1/0

-- If we have no jump moves, we only return the simple moves. If there is at least one jump move, we only return the jump moves as to be within the 
-- rules of the game the AI has to make a jump move if one is available.
getallmoves :: ([Move], [Move]) -> [Move]
getallmoves (sm, []) = sm
getallmoves (_, jm) = jm

-- Our min max search that calls our a-b pruning function, with our gamestate, depth, top and bottom values, and True as our initial Boolean value, which 
-- just tells us if we are maximizing or minimizing.
minimax :: GameState -> Int -> (Float, Maybe Move)
minimax g depth = alphabeta g depth (-infinity) (infinity) True


-- This is our a-b pruning function. It uses our heuristics to find the best moves through the given depth, and eventually we return
-- (value of move, Maybe move).
alphabeta :: GameState -> Int -> Float -> Float -> Bool -> (Float, Maybe Move)
alphabeta g depth alpha beta maximizing 
    -- if the game is over we return Nothing as there are no moves.
    | status g == GameOver = (heuristic g, Nothing)
    -- if the depth is <= 0, we also return Nothing as our move because we have gone to the needed depth
    | depth <= 0 = (heuristic g, Nothing)
    -- if we are maximizing
    | maximizing = let 
            -- This function is our maximizing search
            maximize :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
            -- if we have no moves then return what we had
            maximize [] ret = ret
            maximize (m:ms) (prevAlpha, (prevValue, prevMove)) 
                | alpha >= beta = (alpha, (value, move))
                | otherwise = maximize ms (alpha, (value, move))
                where 
                    -- calling alphabeta with our new gamestate after applying our move and decrementing depth by 1. eval will be the value of this gamestate
                    (eval, _) = alphabeta (apply_move m g) (depth - 1)  prevAlpha beta False
                    value = max prevValue eval
                    alpha = max prevAlpha value
                    move = if prevValue == value then prevMove else Just m
               -- our "main" call in this part of the guard
            in snd $ maximize (getallmoves (moves g)) (alpha, (-infinity, Nothing))
    -- we are not maximizing, so we are minimizing
    | otherwise = snd $ minimize (getallmoves (moves g)) (beta, (infinity, Nothing))
            where
                -- This function is our minimizing search
                minimize :: [Move] -> (Float, (Float, Maybe Move)) -> (Float, (Float, Maybe Move))
                -- if we have no moves then return what we had
                minimize [] ret = ret
                minimize (m:ms) (prevBeta, (prevValue, prevMove)) 
                    | beta <= alpha  = (beta, (value, move))
                    | otherwise = minimize ms (beta, (value, move))
                    where 
                        -- calling alphabeta with our new gamestate after applying our move and decrementing depth by 1. eval will be the value of this gamestate
                        (eval, _) = alphabeta (apply_move m g) (depth - 1) alpha prevBeta False
                        value = min prevValue eval
                        beta = min prevBeta value
                        move = if prevValue == value then prevMove else Just m