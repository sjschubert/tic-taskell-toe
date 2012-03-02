module Main (main) where

import Control.Monad
import Data.Char
import System.Exit
import System.IO

import TTT.Game

main :: IO ()
main = do

  putStr "-----------------"
  putStrLn "\n Tic Taskell Toe \n"
  putStrLn help

  loop initBoard

  where 
    loop b = do
        s <- interactive b
        putStrLn "stepped game by one"
        loop s

interactive :: Board -> IO Board
interactive b = do 
  putStr "Enter index: "
  
  c <- getChar
  l <- getLine -- chew up any reamining character

  case c of 
    i | i == 'q' -> exitSuccess
      | i == '?' -> (putStr help) >> return b
      | (\d -> d `elem` ['0'..'9']) i -> do
          -- todo: stepgame using input 
          putStr $ renderBoard b 
          return b
      | otherwise -> (putStrLn "Invalid choice! Type '?' for help...") >> return b
  
renderBoard :: Board -> String
renderBoard b = 
    " **Board State**\n\n" ++
    "  " ++ (renderPosition $ topLeft b) ++ "  |" ++
    "  " ++ (renderPosition $ topCenter b) ++ "  |" ++
    "  " ++ (renderPosition $ topRight b) ++
    "\n_____|_____|_____\n" ++
    "  " ++ (renderPosition $ middleLeft b) ++ "  |" ++
    "  " ++ (renderPosition $ middleCenter b) ++ "  |" ++
    "  " ++ (renderPosition $ middleRight b) ++
    "\n_____|_____|_____\n" ++
    "  " ++ (renderPosition $ bottomLeft b) ++ "  |" ++
    "  " ++ (renderPosition $ bottomCenter b) ++ "  |" ++
    "  " ++ (renderPosition $ bottomRight b) ++ 
    "\n     |     |     \n\n"
  where
    renderPosition p = 
      case p of
        X     -> "X"
        O     -> "O"
        Empty -> " "
    
help :: String
help = "   **Indexes**\n\n" ++
    "  1  |" ++ "  2  |" ++"  3" ++
    "\n_____|_____|_____\n" ++
    "  4  |" ++ "  5  |" ++"  6" ++
    "\n_____|_____|_____\n" ++
    "  7  |" ++ "  8  |" ++"  9" ++
    "\n     |     |     \n\n" 