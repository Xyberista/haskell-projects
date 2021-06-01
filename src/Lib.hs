module Lib
    ( runTicTacToe
    ) where

import qualified TicTacToe as Tic

runTicTacToe :: IO ()
runTicTacToe = Tic.runGame
