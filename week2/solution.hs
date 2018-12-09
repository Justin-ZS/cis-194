{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown      (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))
         
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

canMove :: Tile -> Bool
canMove Ground = True
canMove Storage = True
canMove _ = False

data Direction = R | U | L | D

data Coord = C Integer Integer

tryMove :: Direction -> Coord -> Coord
tryMove dir prev
  | canMove $ maze next = next
  | otherwise = prev
  where next = adjacentCoord dir prev
        
tryMove2 :: Direction -> Coord -> State
tryMove2 dir prev
  | canMove $ maze next = State dir next
  | otherwise = State dir prev
  where next = adjacentCoord dir prev

player :: Picture
player = translated 0 0.3 cranium
       & path [(0,0),(0.3,0.05)] 
       & path [(0,0),(0.3,-0.05)] 
       & path [(0,-0.2),(0,0.1)] 
       & path [(0,-0.2),(0.1,-0.5)]
       & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18 & sector (7/6*pi) (1/6*pi) 0.18

player2 :: Direction -> Picture
player2 R = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player2 L = scaled (-1) 1 (player2 R) -- Cunning!
player2 U = translated 0 0.3 cranium
          & path [(0,0),(0.3,0.05)] 
          & path [(0,0),(-0.3,0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player2 D = translated 0 0.3 cranium
          & path [(0,0),(0.3,-0.05)] 
          & path [(0,0),(-0.3,-0.05)] 
          & path [(0,-0.2),(0,0.1)] 
          & path [(0,-0.2),(0.1,-0.5)]
          & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)


initialCoord :: Coord
initialCoord = C 0 1

data State = State Direction Coord
initialCoord2 :: State
initialCoord2 = State R $ C 0 1


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) player & pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

handleTime2 :: Double -> State -> State
handleTime2 _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = tryMove R c
    | key == "Up"    = tryMove U c
    | key == "Left"  = tryMove L c
    | key == "Down"  = tryMove D c
    | otherwise      = c
handleEvent _ c      = c

handleEvent2 :: Event -> State -> State
handleEvent2 (KeyPress key) s@(State _ c)
    | key == "Right" = tryMove2 R c
    | key == "Up"    = tryMove2 U c
    | key == "Left"  = tryMove2 L c
    | key == "Down"  = tryMove2 D c
    | otherwise      = s
handleEvent2 _ s      = s


drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze

drawState2 :: State -> Picture
drawState2 (State dir (C x y)) = translated (fromIntegral x) (fromIntegral y) (player2 dir) & pictureOfMaze

resetableInteractionOf ::
    world ->
    (Double -> world -> world) ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableInteractionOf defaultState timeReducer eventReducer display 
  = interactionOf defaultState timeReducer eventReducer2 display
  where 
  eventReducer2 (KeyPress key) s 
    | key == "Esc" = defaultState 
  eventReducer2 e s = eventReducer e s

main :: IO ()
-- main = interactionOf initialCoord handleTime handleEvent drawState              %%exercise 1
-- main = interactionOf initialCoord2 handleTime2 handleEvent2 drawState2          %%exercise 2
main = resetableInteractionOf initialCoord2 handleTime2 handleEvent2 drawState2 -- %%exercise 3