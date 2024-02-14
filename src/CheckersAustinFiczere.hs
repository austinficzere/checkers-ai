module CheckersAustinFiczere (moves, apply_move, red_ai, black_ai)

where

import Checkers.Types

-- This function is our "main" function of this file. It calls simple_moves and jump_moves to get all the valid simple and jump moves available to the 
-- current player. We will use this to determine if the move the player makes is valid or not.
moves :: GameState -> ([Move],[Move])
moves g = (simple_moves g, jump_moves g)

-- Gets all the simple moves for the current player whose turn it is. We call simpleKing and simplePiece to get all the simple moves for normal pieces
-- and kings, and then append these two lists together to get all the simple moves. If it is neither players move, then the game must be over and thus
-- we return the empty list as there are no moves.
simple_moves :: GameState -> [Move]
simple_moves g
    | _status g == Red = (simpleKing (redKings g) g) ++ (simplePiece (redPieces g) g)
    | _status g == Black = (simpleKing (blackKings g) g) ++ (simplePiece (blackPieces g) g)
    | otherwise = []

-- This function takes a GameState and returns the Player whose turn it is, either Red or Black. We use status from GameState to get whose Turn it is,
-- and if its Turn Red, we return Red. Otherwise its Blacks turn so we return Black.
_status :: GameState -> Player
_status g
    | status g == Turn Red = Red
    | otherwise = Black

-- This function gets all the valid simple moves for kings that are passed to it. We use a list comprehension. We take the coord from the list
-- that was given to us as (x, y) and then get the four squares that the king can possibly move to. We make sure the move won't cause a repeated gamestate, 
-- and also check the move is on the board and isnt already occupied. If the move is valid, it gets added to the list, so at the end we have all the possible 
-- valid simple moves for each king.
simpleKing :: [Coord] -> GameState -> [[PorK Coord]]
simpleKing xs g = [ [K (x,y), K (x',y')] | (x,y) <- xs, (x',y') <- [(x+1,y+1), (x-1,y+1), (x+1,y-1), (x-1,y-1)]
                  , notoccupied (x', y') g && onboard (x', y') && check_history ([[K (x,y), K (x', y')]] ++ history g) [] 0 ]

-- This function checks the history of our game to make sure a GameState is not repeated if the move is made. We take history with the potential move, an empty
-- list (movement), and an Int (represents where in the function we want to go). We first check when the Int is 1 or 2 and return True in this situation to follow the
-- algorithm. The next line, we put the first two simple king moves (reds first move and blacks first move) into our movement list and then call the function
-- with Int = 1. This tells us that we have added the moves into movement. Our lines with 1 and 2 check the next move in history, and acts accordingly based on 
-- what things are equal. The lines with 3 and 4 do the exact same thing as the lines with 1 and 2, but if they are found to be "equal" moves, we return false
-- as the GameState is repeated. Our final line just is a final check if every other test was not pattern matched, then our history must have only had 1 move 
-- or the history had a non simple king move, and therefore we return True. So, we can see that if we arrive at a case where history is empty with Int = 1 or 2
-- and movement is not empty, no repeated state took place, and we return True. The general idea of this algorithm came from Alex's powerpoint from Tut10.
check_history :: [Move] -> [Move] -> Int -> Bool
check_history [] _ 1 = True
check_history [] _ 2 = True
check_history ([x,y]:[x',y']:xs) _ 0 = check_history xs [[x,y], [x',y']] 1
check_history ([n1, y]:xs1) ([x, n2]:xs2) 1
    | n1 == n2 = if (x == y) then check_history xs1 xs2 3 else check_history xs1 ([x, y]:xs2) 2
    | otherwise = check_history xs1 ([x, n2]:xs2) 2
check_history ([n1,y]:xs1) (f:[x,n2]:xs2) 2
    | n1 == n2 = if (x == y) then check_history xs1 (f:xs2) 4 else check_history xs1 (f:[x, y]:xs2) 1 
    | otherwise = check_history xs1 (f:[x,n2]:xs2) 1 
check_history ([n1,y]:xs1) ([x,n2]:xs2) 3 
    | n1 == n2 = if (x == y) then False else check_history xs1 ([n1,y]:[x, y]:xs2) 2
    | otherwise = check_history xs1 ([n1,y]:[x,n2]:xs2) 2
check_history ([n1,y]:xs1) ([x,n2]:xs2) 4
    | n1 == n2 = if (x == y) then False else check_history xs1 ([x, y]:[n1,y]:xs2) 1
    | otherwise = check_history xs1 ([x,n2]:[n1,y]:xs2) 1
check_history _ _ _ = True

-- This function gets all the valid simple moves for normal pieces that are passed to it. We use a list comprehension. We take the coord from the list
-- that was given to us as (x', y') and based on whose turn it is we get (x', y') from another list that represents the two squares where the piece can
-- move. We check the coordinates that the piece "might" move to by checking that it is on the board and that a piece is not already occupying it. Finally,
-- if the coords pass all these tests, then we also check if the square that the peiece is moving to is with y' == 0 or 7, and if it is we change the 
-- piece to a King.
simplePiece :: [Coord] -> GameState -> [[PorK Coord]]
simplePiece xs g = [ [P (x,y), (if (y' == 0 || y' == 7) then K else P) (x',y')] | (x,y) <- xs, (x',y') <- let y' = y + dir g in [(x+1,y'), (x-1,y')], notoccupied (x', y') g && onboard (x', y')]

-- Takes a GameState and returns an Int, either 1 or -1 which is the direction on the board we are going. If we are Red, our y value is decreasing as we
-- move up the board, so we return -1. Otherwise we are Black, and we are increasing our y so we return 1.
dir :: GameState -> Int
dir g
    | _status g == Red = -1
    | otherwise = 1

-- This function determines if a set of coordinates is on the gameboard. For it to be on the board, x and y have to be between 0 and 7 inclusive. So,
-- we check for this conditiona and if it is true, we return True. Otherwise the coords are not on the board and we return False.
onboard :: (Int, Int) -> Bool
onboard (x, y)
    | x >= 0 && x <= 7 && y >= 0 && y <= 7 = True
    | otherwise = False

-- This function determines if a coodrinate on the baord is occupied by a piece or not. We use our elem' function to check all the pieces on the board,
-- and if the coord is in any of these lists, we return False, as the square is occupied. Otherwise, the square is not occupied and we return True.
notoccupied :: (Int, Int) -> GameState -> Bool
notoccupied x g
    | x `elem'` (redPieces g) || x `elem'` (redKings g) || x `elem'` (blackPieces g) || x `elem'` (blackKings g) = False
    | otherwise = True

-- This function determines if a given element is in a list. If the list is empty, we return False. Then, we use pattern matching to get the first
-- element of the list and compare to our given element. If they are equal, we return True, and otherwise we call our function recursively with the
-- given element and the rest of the list. So, either we find an equal element or our list eventually becomes empty and we return False.
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
    | x == y = True
    | otherwise = elem' x ys

-- Gets all the jump moves for the current player whose turn it is. We call jumpKing and jumpPiece to get all the jump moves for normal pieces
-- and kings, and then append these two lists together to get all the jump moves. If it is neither players move, then the game must be over and thus
-- we return the empty list as there are no moves.
jump_moves :: GameState -> [Move]
jump_moves g
    | _status g == Red = (jumpKing (redKings g) g) ++ (jumpPiece (redPieces g) g)
    | _status g == Black = (jumpKing (blackKings g) g) ++ (jumpPiece (blackPieces g) g)
    | otherwise = []

-- This function goes through each king and finds the jump moves for it, if there are any. We use a list comprehension to get each king, and use our
-- jumpKing' function to find any jump moves.
jumpKing :: [Coord] -> GameState -> [[PorK Coord]]
jumpKing xs g = [K (x,y):ys | (x,y) <- xs, ys <- jumpKing' (x,y) [] (x,y) g]

-- This function uses list comprehension and recursion to find jump moves (or multiple jump moves) for a king. We get all the squares where the king could
-- jump to, and then make sure the opponent is occupying the square they are jumping, we arent jumping our own piece, we are on the board, the space we are
-- jumping to is empty. Also, if the place we are jumping to is where we started, the not occupied will be false, so that why we have || in this case. We
-- do this recursively, because in checkers we can make multiple jumps on one move, so we need to take this into consideration.
jumpKing' :: (Int, Int) -> [Coord] -> (Int, Int) -> GameState -> [[PorK Coord]]
jumpKing' start rem (x,y) g =
                   [ K (x'',y''):ys
                   | ((x',y'),(x'',y'')) <- [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2)),((x+1,y-1),(x+2,y-2)),((x-1,y-1),(x-2,y-2))]
                   , not ((x',y') `elem'` rem) && opponent_occupied (x',y') g && (start == (x'',y'') || notoccupied (x'',y'') g && onboard (x'',y''))
                   , ys <- jump_over (jumpKing' start ((x',y'):rem) (x'',y'') g) ]

-- When the jumps are over, we must return a list containing the empty list in order to smoothly terminate the recursive calling of the jumps. So, this
-- is the purpose of this function. If the list is still not empty we just return it without changing it, but if it is empty, we return a list with an
-- empty list as its only element.
jump_over :: [[PorK Coord]] -> [[PorK Coord]]
jump_over [] = [[]]
jump_over z = z

-- This function determines if the opponent is occupying the specified square. We find whoes turn it is, and using that we look through the opposite 
-- pieces and check if our coord is in the opponents pieces using elem'. If it is we return True, and if it isn't we return False.
opponent_occupied :: (Int, Int) -> GameState -> Bool
opponent_occupied x g
    | _status g == Red = if x `elem'` (blackKings g ++ blackPieces g) then True else False
    | _status g == Black = if x `elem'` (redKings g ++ redPieces g) then True else False

-- This function goes through each normal piece and finds the jump moves for it, if there are any. We use a list comprehension to get each piece, and use our
-- jumpPiece' function to find any jump moves.
jumpPiece :: [Coord] -> GameState -> [[PorK Coord]]
jumpPiece xs g = [P (x,y):ys | (x,y) <- xs, ys <- jumpPiece' (x,y) [] (x,y) g]

-- This function uses list comprehension and recursion to find jump moves (or multiple jump moves) for a normal piece. We get all the squares where the piece 
-- could jump to, and then make sure the opponent is occupying the square they are jumping, we arent jumping our own piece, we are on the board, the space we 
-- are jumping to is empty. Also, if the place we are jumping to is where we started, the not occupied will be false, so that why we have || in this case. We
-- do this recursively, because in checkers we can make multiple jumps on one move, so we need to take this into consideration. Also, since in our variation 
-- a normal piece can become a king and keep jumping, we check if the piece became a king, and if it did we call jumpKing' instead of jumpPiece' recursively.
-- We also check this property for the PorK datatype so that if we became a king, we have K instead of P.
jumpPiece' :: (Int, Int) -> [Coord] -> (Int, Int) -> GameState -> [[PorK Coord]]
jumpPiece' start rem (x,y) g =
                    [ (if (y'' == 0 || y'' == 7) then K else P) (x'',y''):ys
                    | ((x',y'),(x'',y'')) <- let y' = y + dir g
                                                 y'' = y + dir2 g 
                                                 in [((x+1,y'),(x+2,y'')),((x-1,y'),(x-2,y''))]
                    , not ((x',y') `elem'` rem) && opponent_occupied (x',y') g && (start == (x'',y'') || notoccupied (x'',y'') g && onboard (x'',y''))
                    , ys <- jump_over (if (y'' == 0 || y'' == 7) then jumpKing' start ((x',y'):rem) (x'',y'') g else jumpPiece' start ((x',y'):rem) (x'',y'') g) ]

-- Takes a GameState and returns an Int, either 2 or -2 which is the direction on the board we are going. If we are Red, our y value is decreasing as we
-- move up the board, so we return -2. Otherwise we are Black, and we are increasing our y so we return 2.
dir2 :: GameState -> Int
dir2 g
    | _status g == Red = -2
    | otherwise = 2


-- This function is our "main" function to apply a move to a GameState. If our move is in simple_moves and jump_moves is empty, we call make_simple_move which
-- changes our GameState accordingly, and we also add our move to history after this function returns. If our move is in simple_moves and jump_moves is not 
-- empty, we return the same GameState but with a message suggesting a jump move. If the move is in jump_moves, then we call make_jump_move which changes our 
-- GameState accordingly, and we also add our move to history and change the status to the next player after this function returns. If none of these guards 
-- pass, then the move must have been illegal, so we return the same GameState but with a new message telling the user the move was invalid. Also, if we 
-- call make_simple_move or make_jump_move, we also call isGameOver on our final GameState to see if the game is over or not.
apply_move :: Move -> GameState -> GameState
apply_move m g
    | moves g == ([], []) = g {status = GameOver}
    | m `elem` simple_moves g && jump_moves g == [] = (make_simple_move (convert m) g) {history = [m] ++ history g}
    | m `elem` simple_moves g && jump_moves g /= [] = g {message = "Illegal move! A jump is available:  " ++ (show (jump_moves g))}
    | m `elem` jump_moves g = (make_jump_move (convert m) g) {status = change_player g
                                                              , history = [m] ++ history g}
    | otherwise = g{message = "Illegal move!"}

-- This function applies a simple move to a GameState and returns a new GameState. We use guards for the cases when our piece is part of red/black pieces/kings.
-- In the cases of red/black kings, we replace our start Coord with our end Coord, switch the status to the next player, and change the message to "". In the
-- cases of red/black pieces, we first check if the piece became a king. If it did, we remove the piece from red/blackPieces, add it to red/blackKings, change
-- the player, and change the message to "". If the piece does not become a king, we replace the start Coord with the end Coord in red/blackPieces, change the
-- player, and change the message to "". If for some reason none of these guards are entered, we have a fail safe to report a GameState with the message 
-- invalid make_simple_move. Also, this functions idea and barebones came from Si Zhang's tutorial.
make_simple_move :: PieceState -> GameState -> GameState
make_simple_move [start,end] g
    | status g == (Turn Black) && elem start (blackKings g) 
        = g{blackKings = replace start end (blackKings g)
            , status = change_player g
            , message = ""}
    | status g == (Turn Black) && elem start (blackPieces g)
        = if is_king end (Turn Black)
            then g{blackPieces = remove start (blackPieces g)
                   , blackKings = end:(blackKings g)
                   , status = change_player g
                   , message = ""}
            else g{blackPieces = replace start end (blackPieces g)
                   , status = change_player g
                   , message = ""}
    | status g == (Turn Red) && elem start (redKings g) 
        = g{redKings = replace start end (redKings g)
            , status = change_player g
            , message = ""} 
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king end (Turn Red) 
            then g{redPieces = remove start (redPieces g)
                   , redKings = end:(redKings g)
                   , status = change_player g
                   , message = ""}
            else g{redPieces = replace start end (redPieces g)
                   , status = change_player g
                   , message = ""}
    | otherwise = g{message = "invalid make_simple_move"}

-- This function applies a jump move to a GameState and returns a new GameState. We use guards for the cases when our piece is part of red/black pieces/kings.
-- In the cases of red/black kings, we remove the piece we jumped from the opposite colour king/piece lists as we don't know what type of piece we are jumping, 
-- add the next Coord to our red/blackKings list and remove the start Coord from it, and change the message to "". In the cases of red/black pieces, we first
-- check if the piece became a king. If it did, we remove the piece from red/blackPieces, add it to red/blackKings, remove the piece we jumped from the opposite 
-- colour king/piece lists as we don't know what type of piece we are jumping, and finally change the message to "". If the piece does not become a king, we 
-- remove the start Coord and add the next Coord in red/blackPieces, remove the piece we jumped from the opposite colour king/piece lists as we don't know what 
-- type of piece we are jumping, and change the message to "". In all of these cases, we pass the "updated" GameState to make_jump_move with (next:rest), so 
-- that we keep using the function to update the jump move, as some moves can have multiple jumps. If for some reason none of these guards are entered, we have 
-- a fail safe to report a GameState with the message invalid make_jump_move. Finally, if the pattern match with (start:(next:rest)) fails, we have another 
-- pattern match to catch when the jump is finished. For example, if we have finished our jump and we have called make_jump_move with (next:rest), rest will
-- be [], and so the pattern macth with (start:(next:rest)) fails. So, we know when this match fails, the jump is finished and we return our GameState, which 
-- is what our final pattern match does. Also, this functions idea and barebones came from Si Zhang's tutorial.
make_jump_move :: PieceState -> GameState -> GameState
make_jump_move (start:(next:rest)) g 
    | status g == (Turn Black) && elem start (blackKings g) 
        = make_jump_move (next:rest)
                (g{redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , blackKings = next:(remove start (blackKings g))
                    , message = ""})
    | status g == (Turn Black) && elem start (blackPieces g) 
        = if is_king next (Turn Black)
            then make_jump_move (next:rest)
                (g{blackPieces = remove start (blackPieces g)
                    , redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , blackKings = next:(blackKings g)
                    , message = ""})
            else make_jump_move (next:rest)
                (g{blackPieces = next:(remove start (blackPieces g))
                    , redKings = remove (jumped start next) (redKings g)
                    , redPieces = remove (jumped start next) (redPieces g)
                    , message = ""})
    | status g == (Turn Red) && elem start (redKings g)
        = make_jump_move (next:rest)
                (g{blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , redKings = next:(remove start (redKings g))
                    , message = ""})
    | status g == (Turn Red) && elem start (redPieces g)
        = if is_king next (Turn Red)
            then make_jump_move (next:rest)
                (g{redPieces = remove start (redPieces g)
                    , blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , redKings = next:(redKings g)
                    , message = ""})
            else make_jump_move (next:rest)
                (g{redPieces = next:(remove start (redPieces g))
                    , blackKings = remove (jumped start next) (blackKings g)
                    , blackPieces = remove (jumped start next) (blackPieces g)
                    , message = ""})
    | otherwise = g{message = "invalid make_jump_move"}
make_jump_move _ g = g

-- This function determines if a Coord is a king. Since we take in a Status, if the Status is Turn Red and our y of our Coord is 0, then our piece is a red 
-- king, so we return true. If the Status is Turn Black and our y of our Coord is 7, then our piece is a black king, so we return true. Otherwise, our coord 
-- was not 0 or 7, and so we return false as it is not a king.
is_king :: Coord -> Status -> Bool
is_king (x, y) s
    | s == (Turn Red) && y == 0 = True
    | s == (Turn Black) && y == 7 = True
    | otherwise = False

-- This function simply takes in a two Coords, one we want to replace and the one that we are replacing it with, and the list of Coords we look to replace 
-- our Coord from. We use a simple list comprehension to check if the Coord we want to replace ever shows up, and if it does we replace it with the Coord
-- we want to. If its not, we just put the Coord back into the list.
replace :: Coord -> Coord -> [Coord] -> [Coord]
replace x y xs = [if (z == x) then y else z | z <- xs]

-- This function removes a Coord from a list of Coords. We use a list comprehension, with a condition that if the Coord from our list is not equal to the one
-- we want to remove, it can be in our list comprehension. So, when we encounter the Coord we want to remove, it fails the test and is not put into the list.
remove :: Coord -> [Coord] -> [Coord]
remove c xs = [x | x <- xs, x /= c]

-- This function changes the status of a GameState. If the Turn was Red, we change to Turn Black. Otherwise, the Turn was Black, and we change it to Turn Red.
change_player :: GameState -> Status
change_player g
    | status g == Turn Red = Turn Black
    | otherwise = Turn Red

-- This function finds the square on the board that is getting jumped, using the starting and ending Coords of the piece that jumped it. To find this, 
-- we simply average the x and y values of the start and end of the piece, and that gives us the Coords of the piece that was jumped.
jumped :: Coord -> Coord -> Coord
jumped (x, y) (x', y') = (xavg, yavg) where xavg = (x + x') `div` 2 
                                            yavg = (y + y') `div` 2

-- This function takes in a move and converts it to a PieceState, basically removing the PorK data from the move and leaving us only with the Coord data.
-- We use a list comprehension that goes through each part of the move and uses the change function to remove the PorK data as required.
convert :: Move -> PieceState
convert m =  [change x | x <- m]

-- This function takes in a PorK Coord, removes the PorK data and returns only the Coord data. We simply use pattern matching to cover cases of P and K, 
-- and only return the (x, y) values, or the Coord that we wanted.
change :: PorK Coord -> Coord
change (P (x, y)) = (x, y)
change (K (x, y)) = (x, y)


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


-- Our heuristic function that takes in a GameState and returns a float which represents how "good" that gamestate is for the current player.
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

-- Getting the number of pieces currently on the board, simply adding the lengths of all our piece lists.
numPieces :: GameState -> Int
numPieces g = length (blackPieces g) + length (blackKings g) + length (redPieces g) + length (redKings g)

-- Determine who is "winning" in the current state, by who has more pieces.
winning :: GameState -> Bool
winning g
    | status g == Turn Red = if (length (redPieces g ++ redKings g) >= length (blackPieces g ++ blackKings g)) then True else False
    | otherwise = if (length (blackPieces g ++ blackKings g) >= length (redPieces g ++ redKings g)) then True else False