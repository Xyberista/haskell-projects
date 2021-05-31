module TicTacToe where

import           Data.List
import qualified Data.Map as Map
import           Data.Maybe

data Move = Move { col :: Int
                 , row :: Int
                 , symbol :: String}
            
data GameState = GameState Board

type Board = Map.Map Int Row
type Row = Map.Map Int String

gameToString :: GameState -> String
gameToString (GameState board) =
  intercalate "-----\n" $ Map.foldr (\y r -> ((intercalate "|" $ get y) ++ "\n") : r) [] board
  where get :: Row -> [String]
        get a = Map.foldr (\x c -> (if x == "" then " " else x) : c) [] a

displayBoard :: GameState -> IO ()
displayBoard game = putStrLn $ gameToString game

emptyBoard :: Board
emptyBoard = Map.fromList $ map (\y -> (y, Map.fromList $ map (\x -> (x, "")) [0..2])) [0..2]

makeMove :: Board -> Move -> (Bool, Board)
makeMove board (Move x y sym) =
  if x `elem` [0..2] && y `elem` [0..2] && not taken then
    (True, Map.alter (\r -> Just $ Map.alter (\_ -> Just sym) x $ fromJust r) y board)
  else (False, board)
  where taken = ("" /=) . fromJust . Map.lookup x . fromJust $ Map.lookup y board

-- no tests written for this yet
getMove :: String -> IO Move
getMove sym = do
  putStrLn $ "What is your row number, player " ++ sym ++ "? (0 to 2)"
  putStr ">"
  y <- getLine
  putStrLn $ "What is your column number?"
  putStr ">"
  x <- getLine
  return $ Move (read x) (read y) sym
