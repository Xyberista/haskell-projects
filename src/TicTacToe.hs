module TicTacToe where

import qualified Data.Map as Map
import Data.Maybe

data Move = Move { col :: Int
                 , row :: Int
                 , symbol :: String}
            
newtype GameState = GameState Board

type Board = Map.Map Int Row
type Row = Map.Map Int String

emptyBoard :: Board
emptyBoard = Map.fromList $ map (\y -> (y, Map.fromList $ map (\x -> (x, "")) [0..2])) [0..2]

makeMove :: Board -> Move -> (Bool, Board)
makeMove board (Move x y sym) =
  if not taken then
    (True, Map.alter (\r -> Just $ Map.alter (\_ -> Just sym) x $ fromJust r) y board)
  else (False, board)
  where taken = ("" /=) . fromJust . Map.lookup x . fromJust $ Map.lookup y board
