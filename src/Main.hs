module Main (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import System.Exit
import System.IO

import TTT.AI
import TTT.Game
import TTT.Utils

main :: IO ()
main = do
  putStr "-----------------"
  putStrLn "\n Tic Taskell Toe \n"
  putStrLn help

  gs <- initGame  
  putStrLn $ renderPrettyBoard $ board gs
  
  loop gs

  where 
    loop a = do
        s <- interactive a
        --check s for win state, declare winner / draw, or keep looping
        loop s

interactive :: GameState -> IO GameState
interactive gs = do 
  let p = currentPlayer gs
  if isAI p 
    then do 
      let ns = playFor p gs
      putStrLn $ renderPrettyBoard $ board ns
      return ns
    else do
      putStr "Enter index: "
      hFlush stdout
   
      l <- getLine
      let c = maybeRead l
    
      case c of  
        Just i | i == '?' -> (putStr help) >> return gs
               | (\d -> d `elem` ['1'..'9']) i -> do
                    -- todo: stepgame using input 
                    putStrLn $ renderPrettyBoard $ board gs
                    return gs
               | otherwise -> (putStrLn "Invalid choice!\n") >> return gs
        Nothing -> (putStrLn "Invalid blah!\n") >> return gs
    
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