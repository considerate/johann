{-# LANGUAGE GADTSyntax #-}
module Main where

import Graphics.Gloss (display, animate)
import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Color (makeColor, Color)
import Graphics.Gloss.Data.Picture (circle, color, Picture)
import qualified Graphics.Gloss.Data.Picture as Pic

data Tile a where
  Node :: a -> Int -> Int -> Tile a
  Stack :: [Tile a] -> Tile a
  Chain :: [Tile a] -> Tile a

-- Take a string which is assumed to be a single line and wrap it into multiple lines if necessary
wrap :: Int -> String -> [String]
wrap maxlen xs = if length xs > maxlen then y : wrap maxlen ys else [xs]
  where
    (y, ys) = splitAt maxlen xs

node :: String -> Tile String
node text = Node (unlines ls) width height
  where
    longestLine = 10 -- maximum (fmap length ls)
    height = lineHeight * length ls
    charWidth = 70 -- TODO: base this on font somehow
    width = charWidth * longestLine
    ls = concat (fmap (wrap 30) (lines text))
    lineHeight = 120
    

black :: Color
black = makeColor 0.0 0.0 0.0 1.0

yellow :: Color
yellow = makeColor 1.0 1.0 0.0 1.0

white :: Color
white = makeColor 1.0 1.0 1.0 1.0

windowDisplay :: Display
windowDisplay = FullScreen

background :: Color
background = black

exampleTile :: Tile String
exampleTile = node "node 1"

picture :: Float -> Picture
picture _ = color white $ drawTile exampleTile --color white (circle (sin t * 50.0))

drawTile :: Tile String -> Picture
drawTile tile = case tile of
  Node text width height -> Pic.pictures [Pic.Translate (fromIntegral width / 2) (fromIntegral height / 2) (Pic.rectangleWire (fromIntegral width) (fromIntegral height)),  Pic.text text]
  Chain _ -> Pic.blank
  Stack _ -> Pic.blank

main :: IO ()
main = animate windowDisplay background picture
