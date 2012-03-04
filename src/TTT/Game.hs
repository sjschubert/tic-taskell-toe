module TTT.Game (
  TileContents(..),
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

data TileContents = X | O | Empty deriving (Eq)
data BoardState = XWin | OWin | Draw | Playable deriving (Eq)
type Board = [TileContents] 

data Player = Player {
  isAI :: Bool
} 

data GameState = GameState {
  board :: !Board,
  playerOne :: Player,
  playerTwo :: Player  
}

checkBoard :: Board -> BoardState
checkBoard b = undefined

currentPlayer :: GameState -> Player
currentPlayer gs = 
  if (x == o)
    then playerOne gs
    else playerTwo gs    
  where
    x = foldl (\acc t -> if t == X then (acc + 1) else acc) 0 $ board gs
    o = foldl (\acc t -> if t == O then (acc + 1) else acc) 0 $ board gs

initBoard:: Board
initBoard = foldl (\acc t -> Empty : acc) [] [1..9]
 
playTurn :: Player -> GameState -> Int -> Either String GameState
playTurn p gs i = undefined