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

-- Punto de entrada del programa: genera el mapa, carga assets, crea el estado inicial y lanza el bucle principal de Gloss.
main :: IO ()
main = do
  -- Obtener semilla aleatoria real del sistema
  gen    <- getStdGen
  let (mapGen, gameGen) = split gen
  
  -- Generar mapa, posición inicial, enemigos y posición de sala del boss
  let (tileMap, startPos, enemies, bossRoomPos) = generateMap mapGen
  
  assets <- loadAssets
  
  -- Crear estado inicial con la posición de inicio, enemigos y sala del boss
  let initial = emptyState assets tileMap startPos enemies bossRoomPos gameGen
  
  play
    (InWindow "HASKI RPG" (800, 600) (100,100))
    mossGreen
    60
    initial
    render
    handleInput
    updateWorld