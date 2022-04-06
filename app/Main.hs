module Main(main) where

import Graphics.Gloss

main :: IO ()
main = display window achtergrondKleur teken

window :: Display
window = InWindow "Boom van pythagoras" (1000, 1000) (10, 10)

achtergrondKleur :: Color
achtergrondKleur = white

teken :: Picture
teken = rotate 180 $ tekenBoom 0 550 1000 750 1000

tekenVierkant :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Picture
tekenVierkant x1 y1 x2 y2 x3 y3 x4 y4 = do
  Polygon [(x1,y1), (x2,y2), (x3,y3), (x4,y4)]

tekenBoom :: Float -> Float -> Float -> Float -> Float -> Picture
tekenBoom diepte x1 y1 x2 y2 = do
  let diffX = x2 - x1
      diffY = y1 - y2
      x3 = x2 - diffY
      y3 = y2 - diffX
      x4 = x1 - diffY
      y4 = y1 - diffX
      x5 = x4 + 0.5 * (diffX - diffY)
      y5 = y4 - 0.5 * (diffX + diffY)
  if diepte >= 5
    then do
      Pictures [
        tekenVierkant x1 y1 x2 y2 x3 y3 x4 y4]
  else do
      Pictures [
        tekenVierkant x1 y1 x2 y2 x3 y3 x4 y4,
        tekenBoom (diepte+1) x5 y5 x3 y3,
        tekenBoom (diepte+1) x4 y4 x5 y5]
