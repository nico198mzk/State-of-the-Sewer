module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import WorldGen
import GameState
import Input
import Update
import Render
import Assets

main :: IO ()
main = do
  m      <- generateMap
  assets <- loadAssets
  let initial = emptyState assets m
  play
    (InWindow "HASKI RPG" (800, 600) (100,100))
    black
    60
    initial
    render
    handleInput
    updateWorld