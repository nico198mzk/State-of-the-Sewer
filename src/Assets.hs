-- src/Assets.hs
module Assets where

import Codec.Picture
import Codec.Picture.Types
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

import Graphics.Gloss
import Types

-- Helper: convert DynamicImage → ImageRGBA8
toRGBA8 :: DynamicImage -> Image PixelRGBA8
toRGBA8 (ImageRGBA8 img) = img
toRGBA8 dyn               = convertRGBA8 dyn

-- Convert JuicyPixels ImageRGBA8 → Gloss Picture
fromJP :: DynamicImage -> Picture
fromJP dyn =
  let img = toRGBA8 dyn
      w   = imageWidth img
      h   = imageHeight img
      vec = imageData img
      bs  = BS.pack (V.toList vec)
  in bitmapOfByteString
        w
        h
        (BitmapFormat TopToBottom PxRGBA)
        bs
        False

-- Load PNG safely
loadPNG :: FilePath -> IO Picture
loadPNG fp = do
  e <- readImage fp
  case e of
    Left _     -> return (color red (rectangleSolid 32 32))
    Right dyn  -> return (fromJP dyn)

-- Load all required assets
loadAssets :: IO Assets
loadAssets = do
  p   <- loadPNG "assets/player.png"
  e   <- loadPNG "assets/rat_enemy.png"
  slime <- loadPNG "assets/slime_enemy.png"  -- Nueva: cargar slime
  sword <- loadPNG "assets/wood_sword.png"   -- Nueva: cargar espada
  b <- loadPNG "assets/rat_enemy.png"
  
  --pantalla inicial 
  start <- loadPNG "assets/start_screen.png"
  lore <- loadPNG "assets/lore_screen.png"
  controls <- loadPNG "assets/controls.png"

  -- Cargar 4 variantes de suelo (Piso 1)
  fl0 <- loadPNG "assets/Layer 1_tile2.png"
  fl1 <- loadPNG "assets/Layer 1_tile1.png"
  fl2 <- loadPNG "assets/Layer 1_tile3.png"
  fl3 <- loadPNG "assets/Layer 1_tile4.png"
  
  -- Cargar 4 variantes de suelo (Piso 2)
  fl2_0 <- loadPNG "assets/Layer 2_tiles1.png"
  fl2_1 <- loadPNG "assets/Layer 2_tiles2.png"
  fl2_2 <- loadPNG "assets/Layer 2_tiles3.png"
  fl2_3 <- loadPNG "assets/Layer 2_tiles4.png"
  
  -- Cargar 4 variantes de suelo (Piso 3)
  fl3_0 <- loadPNG "assets/Layer 3_tile1.png"
  fl3_1 <- loadPNG "assets/Layer 3_tile2.png"
  fl3_2 <- loadPNG "assets/Layer 3_tile3.png"
  fl3_3 <- loadPNG "assets/Layer 3_tile4.png"
  
  -- Cargar 4 variantes de muro
  wl0 <- loadPNG "assets/wall0.png"
  wl1 <- loadPNG "assets/wall1.png"
  wl2 <- loadPNG "assets/wall2.png"
  wl3 <- loadPNG "assets/wall3.png"
  
  it  <- loadPNG "assets/item_food.png"
  itemAtk   <- loadPNG "assets/item_atk.png"
  itemSpeed <- loadPNG "assets/item_speed.png"
  
  -- Cargar sprite de escalera
  stairs <- loadPNG "assets/Stairs.png"

  --Pantalla final
  finalScreen <- loadPNG "assets/final_screen.png"

  return Assets
    { aPlayer       = p
    , aEnemy        = e
    , aEnemySlime   = slime  -- Nueva: agregar al Assets
    , aBoss         = b   -- nuevo
    , aTileFloors   = [fl0, fl1, fl2, fl3]
    , aTileFloors2  = [fl2_0, fl2_1, fl2_2, fl2_3]  -- Tiles Piso 2
    , aTileFloors3  = [fl3_0, fl3_1, fl3_2, fl3_3]  -- Tiles Piso 3
    , aTileWalls    = [wl0, wl1, wl2, wl3]  -- Lista de variantes de muro
    , aItemFood     = it
    , aItemAtk     = itemAtk
    , aItemSpeed   = itemSpeed
    , aSword        = sword  -- Nueva: agregar espada
    , aStairs       = stairs  -- Sprite de escalera
    , aFinalScreen  = finalScreen
    , aStartScreen  = start 
    , aLoreScreen   = lore
    , aControlsScreen = controls
    }
  