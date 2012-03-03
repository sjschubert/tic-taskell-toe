module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import System.Exit
import System.IO

import TTT.Game
import TTT.Utils

main :: IO ()
main = do

  --render banner
  putStr "-----------------"
  putStrLn "\n Tic Taskell Toe \n"
  putStrLn help

  gs <- initGame  

  putStrLn $ "\n **Board State**\n\n" ++ renderBoard (board gs) ++ "\n"
  
  loop gs

  where 
    loop a = do
        s <- interactive a
        loop s

interactive :: GameState -> IO GameState
interactive gs = do 
  putStr "Enter index: "
  hFlush stdout

  c <- getChar
  l <- getLine -- chew up any reamining character

  case c of 
    i | i == 'q' -> exitSuccess
      | i == '?' -> (putStr help) >> return gs
      | (\d -> d `elem` ['1'..'9']) i -> do
          -- todo: stepgame using input 
          putStrLn $ "\n **Board State**\n\n" ++ renderBoard (board gs) ++ "\n"
          return gs
      | otherwise -> (putStrLn "Invalid choice! Type '?' for help...") >> return gs
  
    
initGame :: IO GameState
initGame = do 
  putStrLn "What game configuration would you like?"
  putStrLn "First Player is X, second Player is O..."
  putStrLn "  1) Player vs. Player"
  putStrLn "  2) Player vs. AI"
  putStrLn "  3) AI vs. Player"
  
  putStr "Enter choice: "
  hFlush stdout

  c <- getLine 
  let a = maybeRead c

  case a of 
    Just 1  -> return (GameState initBoard (Player False) (Player False))
    Just 2  -> return (GameState initBoard (Player False) (Player True))
    Just 3  -> return (GameState initBoard (Player True) (Player False))
    Nothing -> do
      putStrLn "Invalid choice!"
      initGame
    
  