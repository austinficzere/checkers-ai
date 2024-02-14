module Heuristic where 

import Checkers.Types

-- Our heuristic function that takes in a GameState and returns a float which represents how "good" that gamestate is for the current player.
{-
heuristic :: GameState -> Float
heuristic g 
    -- early game, where there are greater than 22 pieces on the board.
    | numPieces g > 22 = case status g of 
                              -- if reds turn, we want moves that are towards the center and we use a list comprehension to do this
                              Turn Red -> fromIntegral (sum ([1 | (x, y) <- redPieces g, x /= 0 && x /= 7] ++ [0]))
                              -- if blacks turn, we want moves that are towards the center and we use a list comprehension to do this
                              Turn Black -> fromIntegral (sum ([1 | (x, y) <- blackPieces g, x /= 0 && x /= 7] ++ [0]))
    -- middle game, where there are between 22 and 11 pieces on the board.
    | numPieces g <= 22 && numPieces g > 10 = case status g of 
                -- if reds turn, we want moves that are towards the opposite end of the board and prioritize having more pieces than the opponent
                Turn Red -> fromIntegral (sum ([if (y <= 4) then 1 else 0 | (x, y) <- (redKings g ++ redPieces g)] ++ [0]) + (length (redPieces g ++ redKings g) - length (blackPieces g ++ blackKings g)))
                -- if blacks turn, we want moves that are towards the opposite end of the board and prioritize having more pieces than the opponent
                Turn Black -> fromIntegral (sum ([if (y >= 3) then 1 else 0 | (x, y) <- (blackKings g ++ blackPieces g)] ++ [0]) + (length (blackPieces g ++ blackKings g) - length (redPieces g ++ redKings g)))
    -- end game, 10 pieces or less on the baord.
    | numPieces g <= 10 = case status g of 
                               -- if reds turn, if red is winning we prioritize chasing the opponent, and if they are losing we run away.
                               Turn Red -> if (winning g) then fromIntegral (sum ([1 | (x, y) <- (redKings g ++ redPieces g), 
                                                                                       (x', y') <- (blackKings g ++ blackPieces g), 
                                                                                       abs (x-x') < 5 && abs (y-y') < 5] ++ [0]))
                                                          else fromIntegral (sum ([1 | (x, y) <- (redKings g ++ redPieces g), 
                                                                                       (x', y') <- (blackKings g ++ blackPieces g), 
                                                                                       abs (x-x') > 3 && abs (y-y') > 3] ++ [0]))
                               -- if blacks turn, if black is winning we prioritize chasing the opponent, and if they are losing we run away.
                               Turn Black -> if (winning g) then fromIntegral (sum ([1 | (x, y) <- (blackKings g ++ blackPieces g), 
                                                                                         (x', y') <- (redKings g ++ redPieces g), 
                                                                                         abs (x-x') < 5 && abs (y-y') < 5] ++ [0]))
                                                            else fromIntegral (sum ([1 | (x, y) <- (blackKings g ++ blackPieces g), 
                                                                                         (x', y') <- (redKings g ++ redPieces g), 
                                                                                         abs (x-x') > 3 && abs (y-y') > 3] ++ [0]))

-}
-- Getting the number of pieces currently on the board, simply adding the lengths of all our piece lists.
numPieces :: GameState -> Int
numPieces g = length (blackPieces g) + length (blackKings g) + length (redPieces g) + length (redKings g)

-- Determine who is "winning" in the current state, by who has more pieces.
winning :: GameState -> Bool
winning g
    | status g == Turn Red = if (length (redPieces g ++ redKings g) >= length (blackPieces g ++ blackKings g)) then True else False
    | otherwise = if (length (blackPieces g ++ blackKings g) >= length (redPieces g ++ redKings g)) then True else False


heuristic :: GameState -> Float 
heuristic g = case status g of
    Turn Red -> fromIntegral ((length (redPieces g) - length (blackPieces g)) + 2 * (length (redKings g) - length (blackKings g)))
    Turn Black -> fromIntegral ((length (blackPieces g) - length (redPieces g)) + 2 * (length (blackKings g) - length (redKings g)))