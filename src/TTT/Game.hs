module TTT.Game (
  Tile(..),
  Board(..),
  BoardState(..),
  Player(..),
  GameState(..),
  checkBoard,
  currentPlayer,
  initBoard,
  playTurn
) 

where

import Data.List

data Tile = X | O | Empty deriving (Eq)
data BoardState = XWon | OWon | Draw | Playable deriving (Eq)
type Board = [Tile] 

data Player = Player {
  isAI :: Bool
} deriving (Eq)

data GameState = GameState {
  board :: Board,
  playerOne :: Player,
  playerTwo :: Player  
}

checkDim :: Tile -> Board -> [Int] -> Bool
checkDim t b xs = all id $ map (\i -> (b !! i) == t) xs 

checkBoard :: Board -> BoardState
checkBoard b
  | any id $ map (\i -> (b !! i) == Empty) [0..8]   = Playable
  | any id $ map (\xs -> checkDim X b xs) winStates = XWon
  | any id $ map (\xs -> checkDim O b xs) winStates = OWon
  | otherwise                                       = Draw

currentPlayer :: GameState -> Player
currentPlayer gs = 
  if (x == o)
    then playerOne gs
    else playerTwo gs    
  where
    x = foldl (\acc t -> if t == X then (acc + 1) else acc) 0 $ board gs
    o = foldl (\acc t -> if t == O then (acc + 1) else acc) 0 $ board gs

initBoard :: Board
initBoard = foldl (\acc t -> Empty : acc) [] [1..9]
 
playTurn :: Player -> GameState -> Int -> Either String GameState
playTurn p gs i
  | canPlay gs i = Right $ gs {board = updateBoard i (playFor gs p) (board gs)}
  | otherwise    = Left $ "Tile " ++ (show i) ++ "is not empty"
  where   
    playFor gs p
      | p == playerOne gs = X
      | otherwise = O

    canPlay gs i = ((board gs) !! i) == Empty

updateBoard :: Int -> Tile -> Board -> Board
updateBoard i e (t:ts)
   | (i == 0)  = e : ts
   | otherwise = t : updateBoard (i-1) e ts

--indexes of win conditions
winStates = [[0,1,2], [3,4,5], [6,7,8], 
             [0,3,6], [1,4,7], [2,5,8],
             [0,4,8], [2,4,6]]