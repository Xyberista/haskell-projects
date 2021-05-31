module TicTacToeSpec (spec) where

import Test.Hspec
import Test.QuickCheck

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
    it "makes move when target slot is empty" $ do
      let board = emptyBoard
      let (possible, newBoard) = makeMove board (Move 0 0 "x")
      possible `shouldBe` True
      fromJust (Map.lookup 0 $ fromJust $ Map.lookup 0 newBoard) `shouldBe` "x"
        
        
        
        
        
