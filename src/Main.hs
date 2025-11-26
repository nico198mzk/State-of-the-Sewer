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

-- Color personalizado para el fondo
mossGreen :: Color
mossGreen = makeColorI 20 45 20 255

main :: IO ()
main = do
  -- Obtener semilla aleatoria real del sistema
  gen    <- getStdGen
  let (mapGen, gameGen) = split gen
  
  -- Generar mapa y obtener posición inicial
  let (tileMap, startPos) = generateMap mapGen
  
  assets <- loadAssets
  
  -- Crear estado inicial con la posición de inicio correcta
  let initial = emptyState assets tileMap startPos gameGen
  
  play
    (InWindow "HASKI RPG" (800, 600) (100,100))
    mossGreen
    60
    initial
    render
    handleInput
    updateWorld