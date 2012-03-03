module TTT.Game (
  TileContents(..),
  BoardState(..),
  Board(..),
  Player(..),
  GameState(..),
  initBoard,
  stepGame
) 

where

data TileContents = X | O | Empty
data BoardState = XWon | OWon | Draw | XTurn | OTurn
type Board = [TileContents] 

data Player = Player {
  isAI :: Bool
} 

data GameState = GameState {
  board :: !Board,
  playerOne :: Player,
  playerTwo :: Player  
}

initBoard:: Board
initBoard = foldl (\acc t -> Empty : acc) [] [1..9]
 
stepGame :: Board -> TileContents -> Int -> Board
stepGame b = undefined