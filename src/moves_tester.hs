module Main where

import Checkers.Types
import Moves


g1 = GameState { blackPieces = [(4,1),(2,1)]
            , redPieces = []
            , blackKings = []
            , redKings = [(5,0)]
            , status = Turn Red
            , message = ""
            , history = []}

 

g2 = GameState {blackPieces = [(6,1),(4,1),(2,1)]
            , redPieces = []
            , blackKings = []
            , redKings = [(7,2)]
            , status = Turn Red
            , message = ""
            , history = []}

 

g3 =  GameState { blackPieces = [(6,3),(6,1),(4,1),(2,1)]
            , redPieces = []
            , blackKings = []
            , redKings = [(5,4)]
            , status = Turn Red
            , message = ""
            , history = []}

 

g4 =  GameState { blackPieces = [(4,3),(6,3),(6,1),(4,1),(2,1)]
            , redPieces = []
            , blackKings = []
            , redKings = [(3,2)]
            , status = Turn Red
            , message = ""
            , history = []}

 

-- Black move to test absearch

g5 = GameState { blackPieces = [(7,0)]
            , redPieces = [(4,5),(6,5)]
            , blackKings = [(4,7)]
            , redKings = [(3,0)]
            , status = Turn Black
            , message = ""
            , history = []}

 

--Red move  move to test absearch

g6 = GameState { blackPieces = [(7,0)]
            , redPieces = [(4,5),(6,5),(4,3),(6,3)]
            , blackKings = [(5,6)]
            , redKings = [(3,0)]
            , status = Turn Red
            , message = ""
            , history = []}

 

-- red move to win!

g7 = GameState { blackPieces = [(7,6)]
            , redPieces = []
            , blackKings = [(0,7)]
            , redKings = [(1,4),(6,7)]
            , status = Turn Red
            , message = ""
            , history = []}

 

t1Black = GameState{ blackPieces = [(3,3),(5,3),(5,5)]
            , redPieces = [(4,4)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

t2Black = GameState{ blackPieces = [(3,3),(5,3),(5,5)]
            , redPieces = [(4,4), (2,6)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

t3Black = GameState{ blackPieces = [(6,6)]
            , redPieces = [(7,7)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

t4Black = GameState{ blackPieces = [(3,3),(5,3),(5,5)]
            , redPieces = [(4,4), (2,6),(4,6),(6,6)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

-- Pawn changing to King

t5Black = GameState{ blackPieces = [(4,6)]
            , redPieces = [(3,3)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

--non-loopy jumps

t6Black = GameState{ blackPieces = [(3,3),(5,3),(5,5)]
            , redPieces = [(4,4), (2,6),(6,6)]
            , blackKings = []
            , redKings = []
            , status = Turn Black
            , message = ""
            , history = []}

 

--Initial Game GameState

t7Black = initialGameState

 

moves_tester [] = putStrLn ""
moves_tester ((name,st,ans):rest) =

      do putStrLn ""
         putStrLn ("Testing "++name++":")
         putStrLn ("     "++ (show (moves st)))
         putStrLn "Should be:"
         putStrLn  ans
         putStrLn ""
         moves_tester rest

 

main = moves_tester [("g1",g1,"[[(5,0),(3,2),(1,0)]]")
                    ,("g2",g2,"[[(7,2),(5,0),(3,2),(1,0)]]")
                    ,("g3",g3,"[[(5,4),(7,2),(5,0),(3,2),(1,0)]]")
                    ,("g4",g4,"[[(3,2),(5,4),(7,2),(5,0),(3,2),(1,0)],[(3,2),(5,0),(7,2),(5,4),(3,2),(1,0)],[(3,2),(1,0)]]")
                    ,("g5",g5,"[[(4,7),(5,6)],[(4,7),(3,6)],[(7,0),(6,1)]]")
                    ,("g6",g6,"[[(3,0),(4,1)],[(3,0),(2,1)],[(4,5),(5,4)],[(4,5),(3,4)],[(6,5),(7,4)],[(6,5),(5,4)],[(4,3),(5,2)],[(4,3),(3,2)],[(6,3),(7,2)],[(6,3),(5,2)]]")
                    ,("g7",g7,"[[(1,4),(2,5)],[(1,4),(0,5)],[(1,4),(2,3)],[(1,4),(0,3)],[(6,7),(5,6)]]")
                    ,("t1Black",t1Black,"[[(5,3),(3,5)]]")
                    ,("t2Black",t2Black,"[[(5,3),(3,5),(1,7)]]")
                    ,("t3Black",t3Black,"[[(6,6),(5,7)]]")
                    ,("t4Black",t4Black,"[[(5,3),(3,5),(5,7),(7,5)],[(5,3),(3,5),(1,7)],[(5,5),(7,7)],[(5,5),(3,7),(1,5)]]")
                    ,("t5Black",t5Black,"[[(4,6),(5,7)],[(4,6),(3,7)]]")
                    ,("t6Black",t6Black,"[[(5,3),(3,5),(1,7)],[(5,5),(7,7)]]")
                    ,("t7Black",t7Black,"[[(0,5),(1,4)],[(2,5),(3,4)],[(2,5),(1,4)],[(4,5),(5,4)],[(4,5),(3,4)],[(6,5),(7,4)],[(6,5),(5,4)]]")]


{-
check_history :: [Move] -> [Move] -> Int -> Bool
check_history [] [] _ = True
check_history [] _ 1 = True
check_history [] _ 2 = True
check_history ([K (x1, y1),K (x1', y1')]:[K (x2, y2),K (x2', y2')]:xs) _ 0 = check_history xs [[K (x1, y1),K (x1', y1')], [K (x2, y2),K (x2', y2')]] 1
check_history ([K (x1, y1),K (x1', y1')]:xs1) ([K (x2, y2),K (x2', y2')]:xs2) 1 = if (x2 == x1' && y2 == y1') 
                                                                                  then check_history xs1 xs2 3 
                                                                                  else check_history xs1 ([K (x2, y2),K (x1', y1')]:xs2) 2
check_history ([K (x1, y1),K (x1', y1')]:xs1) (x:[K (x2, y2),K (x2', y2')]:xs2) 2 = if (x2 == x1' && y2 == y1') 
                                                                                    then check_history xs1 (x:xs2) 4 
                                                                                    else check_history xs1 (x:[K (x2, y2),K (x1', y1')]:xs2) 1
check_history ([K (x1, y1),K (x1', y1')]:xs1) ([K (x2, y2),K (x2', y2')]:xs2) 3 
    | xs1 == [] = if (x2 == x1' && y2 == y1') then False else True
    | otherwise = check_history xs1 ([K (x1, y1),K (x1', y1')]:[K (x2, y2),K (x2', y2')]:xs2) 2
check_history ([K (x1, y1),K (x1', y1')]:xs1) ([K (x2, y2),K (x2', y2')]:xs2) 4
    | xs1 == [] = if (x2 == x1' && y2 == y1') then False else True
    | otherwise = check_history xs1 ([K (x2, y2),K (x2', y2')]:[K (x1, y1),K (x1', y1')]:xs2) 1
check_history _ _ 0 = True


simpleKing :: [Coord] -> GameState -> [[PorK Coord]]
simpleKing xs g = [ [K (x,y), K (x',y')] | (x,y) <- xs, (x',y') <- [(x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]
                  , notoccupied (x', y') g && onboard (x', y') && check_history (history g ++ [[K (x,y), K (x', y')]]) [] 0 ]
-}


{-
-- This function gets all the valid simple moves for kings that are passed to it. We use a list comprehension. We take the coord from the list
-- that was given to us as (x, y) and then get the four squares that the king can possibly move to. We make sure the move won't cause a repeated gamestate, 
-- and also check the move is on the board and isnt already occupied. If the move is valid, it gets added to the list, so at the end we have all the possible 
-- valid simple moves for each king.
simpleKing :: [Coord] -> GameState -> [[PorK Coord]]
simpleKing xs g = [ [K (x,y), K (x',y')] | (x,y) <- xs, (x',y') <- [(x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]
                  , notoccupied (x', y') g && onboard (x', y') && check_history ([[K (x,y), K (x', y')]] ++ history g) [] 0 ]




recursive_history :: [Move] -> Move -> Bool
recursive_history [] _ = True
recursive_history xs m
    | check_history ([m] ++ xs) [] 0 == False = False
    | otherwise = recursive_history (init xs) m


-}
