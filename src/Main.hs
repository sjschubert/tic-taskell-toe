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
  hFlush stdout

  c <- getChar
  l <- getLine -- chew up any reamining character

  case c of 
    i | i == 'q' -> exitSuccess
      | i == '?' -> (putStr help) >> return b
      | (\d -> d `elem` ['1'..'9']) i -> do
          -- todo: stepgame using input 
          putStr $ renderBoard b 
          return b
      | otherwise -> (putStrLn "Invalid choice! Type '?' for help...") >> return b
  
renderBoard :: Board -> String
renderBoard b = 
    " **Board State**\n\n" ++
    "  " ++ (renderPosition $ b !! 0) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 1) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 2) ++
    "\n_____|_____|_____\n" ++
    "  " ++ (renderPosition $ b !! 3) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 4) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 5) ++
    "\n_____|_____|_____\n" ++
    "  " ++ (renderPosition $ b !! 6) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 7) ++ "  |" ++
    "  " ++ (renderPosition $ b !! 8) ++ 
    "\n     |     |     \n\n"
  where
    renderPosition c = 
      case c of
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