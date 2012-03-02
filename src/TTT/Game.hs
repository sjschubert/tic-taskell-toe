module TTT.Game (
  TileContents(..),
  Board(..),
  initBoard,
  stepGame
) 

where

data TileContents = X | O | Empty
data BoardState = XWon | OWon | Draw | XTurn | OTurn
type Board = [TileContents] 

initBoard:: Board
initBoard = foldl (\acc t -> Empty : acc) [] [1..9]
 
stepGame :: Board -> TileContents -> Int -> Board
stepGame b = undefined
  


