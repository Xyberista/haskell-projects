module Main where

import Lib

import Data.Char
import System.Console.ANSI

main :: IO ()
main = do
  putStrLn "      Main menu:"
  putStrLn "======================"
  putStrLn "1) Tic-Tac-Toe"
  putStrLn "Q) Quit program"
  putStrLn "======================"
  putStrLn "What is your choice?"
  choice <- getLine
  case map toLower choice of "1" -> clearScreen >> runTicTacToe
                             "q" -> return ()
                             _   -> putStrLn "Please enter a valid choice" >> clearScreen >> main
  
  
