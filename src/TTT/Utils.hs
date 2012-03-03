module TTT.Utils (
  help,
  isValidChoice,
  maybeRead,
  renderTile,
  renderBoard
) 

where

import Data.Char
import Data.List
import Data.List.Split

import TTT.Game

help :: String
help = "   **Indexes**\n\n" ++
       "  1  |" ++ "  2  |" ++"  3" ++
     "\n_____|_____|_____\n" ++
       "  4  |" ++ "  5  |" ++"  6" ++
     "\n_____|_____|_____\n" ++
      "  7  |" ++ "  8  |" ++ "  9" ++
     "\n     |     |     \n" 

isValidChoice :: Int -> [Int] -> Bool
isValidChoice x r = (x `elem` r)

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, "")] -> Just x
  _         -> Nothing
  
renderBoard :: Board -> String
renderBoard b = 
    intercalate "\n_____|_____|_____\n" $ 
      map (intercalate "|") $
      splitEvery 3 $ 
      map (\x -> "  " ++ (renderTile x) ++ "  ") b

renderTile :: TileContents -> String
renderTile c = 
  case c of
    X     -> "X"
    O     -> "O"
    Empty -> "-"
    