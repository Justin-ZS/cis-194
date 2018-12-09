{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise1

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 0 (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7.5

trafficLight :: String -> Picture
trafficLight "green" = botCircle green & midCircle black & topCircle black & frame
trafficLight "yellow" = botCircle black & midCircle yellow & topCircle black & frame
trafficLight "red" = botCircle black & midCircle black & topCircle red & frame
trafficLight "red and yellow" = botCircle black & midCircle yellow & topCircle red & frame


trafficController :: Double -> Picture
trafficController t
  | time <= 1 = trafficLight "green"
  | time == 2 = trafficLight "yellow"
  | time <= 4 = trafficLight "red"
  | otherwise = trafficLight "red and yellow"
  where time = round (t/3) `mod` 6

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2

tree :: Picture -> Integer -> Picture
tree p 0 = p
tree p n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree p (n-1)) & rotated (- pi/10) (tree p (n-1)))
  
  
flower :: Double -> Picture
flower radius = colored yellow (solidCircle radius)

blossom :: Double -> Picture
blossom t = flower radius
  where radius = min (t / 20) 0.5

blossomTree :: Double -> Picture
blossomTree t = tree (blossom t) 9

exercise2 :: IO ()
exercise2 = animationOf blossomTree
-- Exercise 3

square, wall, ground, storage, box :: Picture
square = solidRectangle 1 1
wall = colored (grey 0.1) square
ground =  colored yellow square
storage = solidCircle 0.4 & ground
box =     colored brown square

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

drawRow :: Integer -> Picture
drawRow (-11) = blank
drawRow x = drawCol x 10 & drawRow (x - 1)

drawAt :: Integer -> Integer -> Picture
drawAt x y = translated (fromIntegral x) (fromIntegral y) $ drawTile $ maze x y

drawCol :: Integer -> Integer -> Picture
drawCol _ (-11) = blank
drawCol x y = drawAt x y & drawCol x (y - 1)

pictureOfMaze :: Picture
pictureOfMaze = drawRow 10

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
         
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2