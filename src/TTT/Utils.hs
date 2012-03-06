module TTT.Utils (
  help,
  maybeRead,
  promptPlayer,
  renderBoard,
  renderPrettyBoard,
  renderTile,
  renderWinner
) 

where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

import TTT.Game

help :: String
help = "Try to get three X's or O's in a row horizontally, vertically, or diagonally.\n" ++
       " -type the desired index when prompted to play to the corresponding tile\n" ++
       " -type 'ctrl + c' to quit\n\n" ++
       "   **Indexes**\n\n" ++
       "  1  |" ++ "  2  |" ++"  3" ++
       "\n_____|_____|_____\n" ++
       "  4  |" ++ "  5  |" ++"  6" ++
       "\n_____|_____|_____\n" ++
       "  7  |" ++ "  8  |" ++ "  9" ++
       "\n     |     |     \n\n"
     

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

promptPlayer :: Player -> String
promptPlayer p = "Enter index(Player " ++ show (playerId p) ++ "): "

renderBoard :: Board -> String
renderBoard b = 
    intercalate "\n_____|_____|_____\n" $ 
      map (intercalate "|") $
      splitEvery 3 $ 
      map (\x -> "  " ++ (renderTile x) ++ "  ") b

renderPrettyBoard :: Board -> String
renderPrettyBoard b = "\n **Board State**\n\n" ++ (renderBoard b) ++ "\n"

renderTile :: Tile -> String
renderTile c = 
  case c of
    X     -> "X"
    O     -> "O"
    Empty -> "-"

renderWinner :: BoardState -> Board -> String
renderWinner bs b 
  | bs == XWon     = "\n   ** X Wins **\n\n" ++ (renderBoard b) ++ "\n"
  | bs == OWon     = "\n   ** O Wins **\n\n" ++ (renderBoard b) ++ "\n"
  | bs == Draw     = "\n   ** Draw **\n\n" ++ (renderBoard b) ++ "\n"
  | bs == Playable = "Game terminated with board in a playable state!"
    