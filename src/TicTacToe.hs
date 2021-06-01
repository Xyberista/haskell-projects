module TicTacToe where

import           Control.Monad.IO.Class
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe

data Move = Move { col :: Int
                 , row :: Int
                 , symbol :: String}

type Board = Map.Map Int Row
type Row = Map.Map Int String
  
instance Eq Move where
  (Move a b c) == (Move d e f) = a == d && b == e && c == f
  
instance Show Move where
  show (Move x y sym) = show x ++ show y ++ sym

gameToString :: Board -> String
gameToString board =
  intercalate "-----\n" $ Map.foldr (\y r -> ((intercalate "|" $ get y) ++ "\n") : r) [] board
  where get :: Row -> [String]
        get a = Map.foldr (\x c -> (if x == "" then " " else x) : c) [] a

displayBoard :: Board -> IO ()
displayBoard game = putStrLn $ gameToString game

emptyBoard :: Board
emptyBoard = Map.fromList $ map (\y -> (y, Map.fromList $ map (\x -> (x, "")) [0..2])) [0..2]

makeMove :: Board -> Move -> (Bool, Board)
makeMove board (Move x y sym) =
  if x `elem` [0..2] && y `elem` [0..2] && not taken then
    (True, Map.alter (\r -> Just $ Map.alter (\_ -> Just sym) x $ fromJust r) y board)
  else (False, board)
  where taken = ("" /=) . fromJust . Map.lookup x . fromJust $ Map.lookup y board

getRow :: String -> IO Int
getRow sym = do
  putStrLn $ "What is your row number, player " ++ sym ++ "? (0 to 2)"
  putStr ">"
  y <- getLine
  if y `elem` ["0","1","2"] then return $ read y
  else putStrLn "Please enter a valid row number." >> getRow sym

getCol :: IO Int
getCol = do
  putStrLn $ "What is your column number? (0 to 2)"
  putStr ">"
  x <- getLine
  if x `elem` ["0","1","2"] then return $ read x
  else putStrLn "Please enter a valid row number." >> getCol
  
getMove :: String -> (String -> IO Int) -> IO Int -> IO Move
getMove sym getR getC = do
  x <- getR sym
  y <- getC
  return $ Move x y sym

getStarter :: IO String
getStarter = do
  putStrLn "Who will start the game? (x or o)"
  putStr ">"
  starter <- getLine
  case starter of "x" -> return "x"
                  "o" -> return "o"
                  _   -> putStrLn "please enter a valid starter" >> getStarter

checkForWin :: Board -> String -> Bool
checkForWin board player =
  any (all (==player)) rows || any (all (==player)) columns
  ||  all (==player) diagonalOne || all (==player) diagonalTwo
  where rowsList = Map.foldr (\x res -> Map.foldr (\y r -> y : r) [] x : res) [] board
        rows = Map.foldr (\y res -> Map.foldr (\x c -> x : c) [] y : res) [] board
        columns = foldr (\y r -> map (\x -> x !! y) rowsList : r) [] [0..2]
        diagonalOne = zipWith (\y r -> r !! y) [0,1,2] rowsList
        diagonalTwo = zipWith (\y r -> r !! y) [2,1,0] rowsList

checkPlayerOne :: Board -> Bool
checkPlayerOne board = checkForWin board "x"

checkPlayerTwo :: Board -> Bool
checkPlayerTwo board = checkForWin board "o"
  
-- todo: implement both functions below
step :: Board -> String -> IO Board
step board player = do
  let a = getMove player getRow getCol 
  return board

runGame :: IO ()
runGame = do
  starter <- getStarter
  let board = emptyBoard
  displayBoard board
  
  step board starter
  return ()
  
  
  
