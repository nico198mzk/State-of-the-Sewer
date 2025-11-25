module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (getStdGen, split)

import WorldGen
import GameState
import Input
import Update
import Render
import Assets

main :: IO ()
main = do
  -- Obtener semilla aleatoria real del sistema
  gen    <- getStdGen
  let (mapGen, gameGen) = split gen  -- Dividir: uno para el mapa, otro para el juego
  let m = generateMap mapGen
  assets <- loadAssets
  let initial = emptyState assets m gameGen
  play
    (InWindow "HASKI RPG" (800, 600) (100,100))
    black
    60
    initial
    render
    handleInput
    updateWorld