{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.IORef
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Identity
import Data.Function
import Data.Proxy
import Data.Tuple.Extra hiding ((&&&))

import FRP.Yampa
import qualified SDL 
import SDL (Window, Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))

import Foreign.C.Types

import System.Random
import Debug.Trace


------------
--- TODO ---
------------

-- Add a timer in the corner
-- Pipe gap vary within a range
-- Pipe speed increase and pipe gap narrow over time

  
----------------
--- GRAPHICS ---
----------------
-- All Objects are center positioned with (0,0) in the lower left corner and Y increasing upward

data Anchor = Center | UpperLeft
data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown deriving Show
data Shape a = Rectangle { _rectW :: a, _rectH :: a}
           --  | Circle    { _rad :: a }
             deriving (Show, Functor)

data Object (anchor :: Anchor) a = Object
  { _shape :: Shape a
  , _pos   :: (a, a)
  , _color :: Color
  } deriving (Show, Functor)

data Scene = Scene
  { _background :: [Object 'Center Double]
  , _foreground :: [Object 'Center Double]
  }

class ToObject a where
  type FunctorType a :: * -> *
  toObject :: a -> FunctorType a (Object 'Center Double)

instance ToObject Cube where
  type FunctorType Cube = Identity
  toObject :: Cube -> Identity (Object 'Center Double)
  toObject (Cube y _) = Identity $ Object (Rectangle cubeSize cubeSize) (cubeX, y) Yellow

instance ToObject Pipe where
  type FunctorType Pipe = []
  toObject :: Pipe -> [] (Object 'Center Double)
  toObject (Pipe x h) =
    let
      bottomWidth = pipeWidth
      bottomHeight = h
      bottomX = x
      bottomY = half groundHeight + half h
      topWidth = pipeWidth
      topHeight = windowH - (groundHeight + h + pipeGap)
      topX = x
      topY = groundHeight + h + pipeGap + half topHeight
    in [ Object (Rectangle bottomWidth bottomHeight) (bottomX, bottomY) Green
       , Object (Rectangle topWidth topHeight) (topX, topY) Green
       ]

instance ToObject Game where
  type FunctorType Game = []
  toObject :: Game -> [Object 'Center Double]
  toObject (Game cube pipe) = toObject pipe ++ (pure . runIdentity) (toObject cube) 

class Functor f => ToScene f where
  toScene :: f (Object 'Center Double) -> Scene

instance ToScene [] where
  toScene :: [Object 'Center Double] -> Scene
  toScene = initScene

instance ToScene Identity where
  toScene :: Identity (Object 'Center Double) -> Scene
  toScene (Identity obj) = initScene (pure obj)

initScene :: [Object 'Center Double] -> Scene
initScene = Scene [Object (Rectangle 200 500) (100, 350) BabyBlue, Object (Rectangle 200 100) (100, 50) Brown]

-- Axis Aligned Rectangle Collision
detectCollision :: Object 'UpperLeft Double -> Object 'UpperLeft Double -> Bool
detectCollision obj1 obj2 =
  let (x1, y1) = _pos obj1
      (x2, y2) = _pos obj2
      (w1, h1) = (_rectW &&& _rectH) (_shape obj1)
      (w2, h2) = (_rectW &&& _rectH) (_shape obj2)
  in x1 < x2 + w2 && x1 + w1 > x2 && y1 > y2 - h2 && y1 - h1 < y2

detectCollision' :: Object 'Center Double -> Object 'Center Double -> Bool
detectCollision' = detectCollision `on` shiftAnchor 


---------------------
--- SDL RENDERING ---
---------------------
-- SDL's coordinate system (0,0) in the top left corner with Y increasing downward
-- SDL.drawRect uses upper Left corner

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 windowW windowH }

shiftAnchor :: Object 'Center Double -> Object 'UpperLeft Double 
shiftAnchor obj =
  let (xC, yC) = _pos obj
      (w, h) = (_rectW &&& _rectH) . _shape $ obj
      (xL, yU) = (xC - half w, yC + half h)
  in obj { _pos = (xL, yU)}

mkObjNum :: (RealFrac a, Num b) => Object anchor a -> Object anchor b
mkObjNum = fmap (fromIntegral . floor)

mkSdlRect :: Num a => Object 'UpperLeft a -> (SDL.Rectangle a, Color)
mkSdlRect obj = 
  let (w, h) = (_rectW &&& _rectH) . _shape $ obj
      (x, y) = (-) windowH <$> _pos obj
      color  = _color obj
  in (SDL.Rectangle (P (V2 x y)) (V2 w h), color)

renderObject :: Renderer -> (SDL.Rectangle CInt, Color) -> IO ()
renderObject r (rect, color) = do
  setDrawColor r color
  SDL.fillRect r (Just rect)

drawObject :: Renderer -> Object 'Center Double -> IO ()
drawObject renderer = shiftAnchor >>> mkObjNum >>> mkSdlRect >>> renderObject renderer

clearFrame :: Renderer -> IO ()
clearFrame renderer = do
  setDrawColor renderer White
  SDL.clear renderer

setDrawColor :: Renderer -> Color -> IO ()
setDrawColor renderer color =
  case color of
    Red      -> SDL.rendererDrawColor renderer $= V4 255 0 0 0
    Blue     -> SDL.rendererDrawColor renderer $= V4 0 0 255 0
    BabyBlue -> SDL.rendererDrawColor renderer $= V4 0 235 255 0
    Green    -> SDL.rendererDrawColor renderer $= V4 120 200 15 0
    White    -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
    Brown    -> SDL.rendererDrawColor renderer $= V4 150 90 25 0
    Yellow   -> SDL.rendererDrawColor renderer $= V4 255 200 50 0
    
drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = setDrawColor renderer color >> SDL.clear renderer

draw :: Renderer -> Scene -> IO ()
draw renderer (Scene bg fg) = do
  clearFrame renderer
  traverse_ (drawObject renderer) (bg ++ fg)
  SDL.present renderer

initSDL :: IO (Renderer, Window)
initSDL = do
  SDL.initializeAll
  window <- SDL.createWindow "Yampy Cube Clone" window
  renderer <- SDL.createRenderer window 0 SDL.defaultRenderer
  return (renderer, window)


----------------
-- USER INPUT --
----------------

keypress :: SF AppInput (Event SDL.Scancode)
keypress = inpKeyPressed ^>> edgeJust

keyPressed :: SDL.Scancode -> SF AppInput (Event ())
keyPressed code = keypress >>^ filterE (code ==) >>^ tagWith ()

data AppInput = AppInput
  { inpKeyPressed :: Maybe SDL.Scancode
  , inpQuit       :: Bool
  }

initAppInput :: AppInput
initAppInput = AppInput Nothing False

parseSDLInput :: SF (Event SDL.EventPayload) AppInput
parseSDLInput = accumHoldBy nextAppInput initAppInput

nextAppInput :: AppInput -> SDL.EventPayload -> AppInput
nextAppInput inp (SDL.KeyboardEvent event) =
  case SDL.keyboardEventKeyMotion event of
    SDL.Pressed  -> inp { inpKeyPressed = Just $ SDL.keysymScancode $ SDL.keyboardEventKeysym event }
    SDL.Released -> inp { inpKeyPressed = Nothing}
nextAppInput inp _ = inp


-------------------------
-- TYPES AND CONSTANTS --  
-------------------------

data Pipe = Pipe { _x :: Double, _h :: Double } deriving Show
data Cube = Cube { _y :: Double, _v :: Double } deriving Show
data Game = Game { gameCube :: Cube, gamePipe :: Pipe }

windowW :: Num a => a
windowW = 200

windowH :: Num a => a
windowH = 600

skyHeight :: Double
skyHeight = 500

groundHeight :: Double
groundHeight = windowH - skyHeight

cubeSize :: Double
cubeSize = 25

cubeX :: Double
cubeX = 50

pipeWidth :: Double
pipeWidth = 40

pipeHeight :: Double
pipeHeight = 100

pipeGap :: Double
pipeGap = 200

initGame :: Game
initGame = Game initCube initPipe

initCube :: Cube
initCube = Cube 350 (-10)

initPipe :: Pipe
initPipe = Pipe 200 100


-----------------
-- GAME LOGIC ---
-----------------

half :: Fractional a => a -> a
half n = n / 2

checkCollision :: Game -> Bool
checkCollision (Game cube pipe) = or $ detectCollision' (runIdentity . toObject $ cube) <$> toObject pipe
  
game :: SF AppInput Game
game = switch sf (const game)
  where
    sf = proc input -> do
      gameState <- gameSession -< input
      gameOver <- edge -< checkCollision gameState
      returnA -< (gameState, gameOver)

gameSession :: SF AppInput Game
gameSession = proc input -> do
  cube <- flappingCube initCube -< input
  pipe <- pipeHeightGen >>> movingPipe initPipe -< ()
  returnA -< Game cube pipe

pipeHeightGen :: SF a Double
pipeHeightGen = noiseR (20, skyHeight - pipeGap - 20) (mkStdGen 3)

movingPipe :: Pipe -> SF Double Pipe
movingPipe (Pipe x0 h0) = switch sf cont
  where
    sf = proc h -> do
      x <- imIntegral x0 -< - 20
      respawn <- edge -< x <= negate cubeSize
      returnA -< (Pipe (x + half pipeWidth) h0, respawn `tag` h)
    cont h = movingPipe $ Pipe x0 h

fallingCube :: Cube -> SF a Cube
fallingCube (Cube y0 v0) = proc _ -> do
  v <- (v0 +) ^<< integral -< -9.81
  y <- (y0 +) ^<< integral -< v
  returnA -< Cube y v

flappingCube :: Cube -> SF AppInput Cube
flappingCube cube = switch sf cont
  where
    sf :: SF AppInput (Cube, Event Cube)
    sf = proc input -> do
      cube <- fallingCube cube -< ()
      flap <- flapTrigger -< input
      returnA -< (cube, flap `tag` cube)
    cont :: Cube -> SF AppInput Cube
    cont (Cube y v) = flappingCube (Cube y 50)

bouncingCube :: Cube -> SF a Cube
bouncingCube cube = switch (sf cube) cont
  where
    sf :: Cube -> SF a (Cube, Event Cube)
    sf cube' = proc input -> do
      Cube y v <- fallingCube cube' -< input
      event <- edge -< y <= -475
      returnA -< (Cube y v, event `tag` Cube y v) 
    cont :: Cube -> SF a Cube
    cont (Cube y v) = bouncingCube (Cube y (-v * 0.7))

flapTrigger :: SF AppInput (Event ())
flapTrigger = proc input -> do
  spaceBarTap <- keyPressed SDL.ScancodeSpace -< input
  returnA -< spaceBarTap

shouldExit :: SF AppInput Bool
shouldExit = quitTrigger >>^ isEvent

quitTrigger :: SF AppInput (Event ())
quitTrigger = proc input -> do
  qButtonTap <- keyPressed SDL.ScancodeQ -< input
  returnA -< qButtonTap

pollKeyboard :: IO (Maybe AppInput)
pollKeyboard = do
  event <- (fmap . fmap) SDL.eventPayload SDL.pollEvent
  case event of
    Just (SDL.KeyboardEvent keyevent) -> do
      let scancode = SDL.keysymScancode . SDL.keyboardEventKeysym $ keyevent
      return $ constructAppInput scancode
    _ -> return Nothing

constructAppInput :: SDL.Scancode -> Maybe AppInput
constructAppInput SDL.ScancodeQ = Just $ initAppInput { inpQuit = True }
constructAppInput SDL.ScancodeSpace = Just $ initAppInput { inpKeyPressed = Just SDL.ScancodeSpace }
constructAppInput _ = Nothing

mainLoop :: IO ()
mainLoop = do
  (renderer, window) <- initSDL
  reactimate (return NoEvent) produceInput (handleOutput renderer) pipeline
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
      produceInput _ = do
        threadDelay 30000
        mevent <- SDL.pollEvent
        case mevent of
          Just event -> return (0.1, Just . Event $ SDL.eventPayload event)
          Nothing -> return (0.1, Nothing)
      handleOutput :: Renderer -> Bool -> (Game, Bool) -> IO Bool
      handleOutput r _ (game, shouldExit) = do
        liftIO $ draw r (toScene $ toObject game)
        return shouldExit
      pipeline :: SF (Event SDL.EventPayload) (Game, Bool)
      pipeline = parseSDLInput >>> (game &&& shouldExit)

  
