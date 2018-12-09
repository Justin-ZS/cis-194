{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)
    
mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x y) (C a b)
  | x == a && y == b = True
  | otherwise = False

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo c1 c2 c3
  | c1 `eqCoord` c3 = c2
  | otherwise = c3

-- The maze

data Tile = Wall | Ground | Storage | Box | Blank
       
maze :: Coord -> Tile 
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c = case maze c of 
  Box -> Ground
  a   -> a

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes Empty c = noBoxMaze c
mazeWithBoxes (Entry c cs) x
  | eqCoord c x = Box
  | otherwise = mazeWithBoxes cs x

-- The state

data State = State Coord Direction (List Coord)


initialBoxes :: List Coord
initialBoxes = go 10 10
  where
    go :: Integer -> Integer -> List Coord
    go (-11) (-10) = Empty
    go (-11) x = go 10 (x - 1)
    go x y = case maze (C x y) of 
      Box -> Entry (C x y) (go (x - 1) y)
      _   -> go (x - 1) y
      
initialBoxes2 :: List Coord
initialBoxes2 = traverse 10 10
  where
    traverse (-11) (-10) = Empty
    traverse (-11) x = traverse 10 (x - 1)
    traverse x y = go x y (traverse (x - 1) y)
      where 
        go x y l = case maze (C x y) of 
          Box -> Entry (C x y) l
          _   -> l

initialState :: State
initialState = State (C 0 1) D initialBoxes

-- Event handling

canMove :: List Coord -> Coord -> Direction -> Bool
canMove cs c d = case currentMaze c of 
  Storage -> True
  Ground  -> True
  Box     -> case currentMaze (adjacentCoord d c) of
    Storage -> True
    Ground  -> True
    _       -> False
  _       -> False 
  where currentMaze = mazeWithBoxes cs

movePlayer :: List Coord -> Coord -> Direction -> Coord
movePlayer boxes c d
  | canMove boxes to d = to
  | otherwise = c
    where to = adjacentCoord d c

moveBox :: Coord -> Direction -> Coord -> Coord
moveBox pc pd = moveFromTo pc  (adjacentCoord pd pc)

move :: State -> Direction -> State
move (State from _ boxes) d = State to d boxes'
  where to = movePlayer boxes from d
        boxes' = mapList (moveBox to d) boxes

handleEvent :: Event -> State -> State
handleEvent _ s
  | isWon s = s
handleEvent (KeyPress key) s
  | key == "Right" = move s R
  | key == "Up"    = move s U
  | key == "Left"  = move s L
  | key == "Down"  = move s D
handleEvent _ s = s

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored (grey 0.4) (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic


player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)] 
         & path [(0,0),(-0.3,0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)] 
         & path [(0,0),(-0.3,-0.05)] 
         & path [(0,-0.2),(0,0.1)] 
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes boxes = combine (mapList (\c -> atCoord c (drawTile Box)) boxes)

drawState :: State -> Picture
drawState (State c d boxes) = atCoord c (player d) & pictureOfBoxes boxes & pictureOfMaze 

drawStateWithWon :: State -> Picture
drawStateWithWon s
  | isWon s = scaled 3 3 (text "You Won!")
  | otherwise = drawState s
  

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawStateWithWon

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen
    
    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)
    
    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)
    
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

-- handle won
isOnStorage :: Coord -> Bool
isOnStorage c = case maze c of
  Storage -> True
  _       -> False

allList :: List Bool -> Bool
allList Empty = True
allList (Entry x xs)
  | x = allList xs
  | otherwise = False

isWon :: State -> Bool
isWon (State _ _ cs) = allList (mapList isOnStorage cs)

-- The main function

main :: IO ()
main = runInteraction (resetable (withStartScreen sokoban))