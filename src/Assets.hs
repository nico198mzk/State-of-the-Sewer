--src/Assets.hs
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
toRGBA8 dyn               = convertRGBA8 dyn   -- siempre funciona

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
        (BitmapFormat TopToBottom PxABGR)
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
  p  <- loadPNG "assets/player.png"
  e  <- loadPNG "assets/rat_enemy.png"
  fl <- loadPNG "assets/tile_floor.png"
  wl <- loadPNG "assets/tile_wall.png"
  it <- loadPNG "assets/item_food.png"

  return Assets
    { aPlayer = p
    , aEnemy = e
    , aTileFloor = fl
    , aTileWall = wl
    , aItemFood = it
    }