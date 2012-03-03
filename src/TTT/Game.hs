module TTT.Game (
  TileContents(..),
  Board(..),
  BoardState(..),
  Player(..),
  GameState(..),
  currentPlayer,
  initBoard,
  stepGame
) 

where

data TileContents = X | O | Empty
                    deriving (Eq)

type Board = [TileContents] 
data BoardState = XWin | OWin | Draw

data Player = Player {
  isAI :: Bool
} 

data GameState = GameState {
  board :: !Board,
  playerOne :: Player,
  playerTwo :: Player  
}

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
 
stepGame :: Board -> TileContents -> Int -> Board
stepGame b = undefined
