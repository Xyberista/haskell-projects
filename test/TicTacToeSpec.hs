module TicTacToeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Maybe

import TicTacToe


spec :: Spec
spec = do
  describe "board creation" $ do
    it "creates empty gameboard" $ do
      let r = Map.fromList $ zip [0..2] (repeat "")
      emptyBoard `shouldBe` Map.fromList [(0,r), (1,r), (2,r)]

  describe "moves" $ do
    context "when the target slot is empty" $ do
      it "move is successfully made" $ do
        let board = emptyBoard
        let (possible, newBoard) = makeMove board (Move 0 0 "x")
        possible `shouldBe` True
        fromJust (Map.lookup 0 $ fromJust $ Map.lookup 0 newBoard) `shouldBe` "x"
  
    context "when the target slot is not empty" $ do
      it "move is unsuccessful, board is unmodified" $ do
        let board = emptyBoard
        let (_, temp) = makeMove board (Move 0 0 "x")
        let (possible, newBoard) = makeMove temp (Move 0 0 "x")
        possible `shouldBe` False
        newBoard `shouldBe` temp
        
    context "when the target slot is out of bounds" $ do
      it "move is unsuccessful, board is unmodified" $ do
        let board = emptyBoard
        let (possible, newBoard) = makeMove board (Move 4 4 "x")
        possible `shouldBe` False
        board `shouldBe` newBoard

  describe "display board" $ do
    context "when empty board" $ do
      it "prints empty board" $ do
        let expected = " | | \n-----\n | | \n-----\n | | \n"
        let game = emptyBoard
        let actual = gameToString game
        actual `shouldBe` expected
        (displayBoard game) >>= \x -> putStrLn expected >>= (`shouldBe` x)
  
    context "when non-empty board" $ do
      it "prints board" $ do
        let expected = "x| | \n-----\n | | \n-----\n | | \n"
        let (_, game) = makeMove emptyBoard (Move 0 0 "x")
        let actual = gameToString game
        actual `shouldBe` expected
        (displayBoard game) >>= \x -> putStrLn expected >>= (`shouldBe` x)

  describe "get move" $ do
    let f :: Int -> IO Int
        f n = return n
    it "returns move" $ do
      let getR _ = f 1
      let getC = f 1
      getMove "x" getR getC `shouldReturn` (Move 1 1 "x")
        

  describe "check for win" $ do
    context "when empty board" $ do
      it "return False" $ do
        let board = emptyBoard
        checkForWin board "x" `shouldBe` False
        checkForWin board "y" `shouldBe` False
        
    context "when non-empty board with no win" $ do
      it "return False" $ do
        let board = emptyBoard
        let (_,newBoard) = makeMove board (Move 0 0 "x")
        displayBoard newBoard
        checkForWin newBoard "x" `shouldBe` False
        checkForWin newBoard "y" `shouldBe` False

    context "when x wins vertically" $ do
      it "return True" $ do
        let board = emptyBoard
        let (_,a) = makeMove board (Move 0 0 "x")
        let (_,b) = makeMove a (Move 0 1 "x")
        let (_,newBoard) = makeMove b (Move 0 2 "x")
        checkForWin newBoard "x" `shouldBe` True
        checkForWin newBoard "y" `shouldBe` False
      
    context "when x wins diagonally" $ do
      it "return True" $ do
        let board = emptyBoard
        let (_,a) = makeMove board (Move 0 0 "x")
        let (_,b) = makeMove a (Move 1 1 "x")
        let (_,newBoard) = makeMove b (Move 2 2 "x")
        checkForWin newBoard "x" `shouldBe` True
        checkForWin newBoard "y" `shouldBe` False

    context "when x wins horizontally" $ do
      it "return True" $ do
        let board = emptyBoard
        let (_,a) = makeMove board (Move 0 0 "x")
        let (_,b) = makeMove a (Move 1 0 "x")
        let (_,newBoard) = makeMove b (Move 2 0 "x")
        checkForWin newBoard "x" `shouldBe` True
        checkForWin newBoard "y" `shouldBe` False
