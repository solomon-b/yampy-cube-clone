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
import Data.Tuple.Extra hiding ((&&&))

import FRP.Yampa
import qualified SDL 
import SDL (Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))

import System.Random
import Debug.Trace

----------------
--- GRAPHICS ---
----------------

data Color = Red | Yellow | Green | Blue | BabyBlue | White | Brown deriving Show
data Shape a = Rectangle { _rectW :: a, _rectH :: a} deriving Show
data Object = Object
  { _shape :: Shape Int
  , _pos   :: (Double, Double)
  , _color :: Color
  } deriving Show

data Scene = Scene
  { _background :: [Object]
  , _foreground :: [Object]
  }

class ToObject a where
  type FunctorType a :: * -> *
  toObject :: a -> FunctorType a Object

instance ToObject Cube where
  type FunctorType Cube = Identity
  toObject :: Cube -> Identity Object
  toObject (Cube y _) = Identity $ Object (Rectangle cubeSize cubeSize) (cubeX, y) Yellow

instance ToObject Pipe where
  type FunctorType Pipe = []
  toObject :: Pipe -> [] Object
  toObject (Pipe x h) =
    [ Object (Rectangle pipeWidth (floor h)) (x, negate 500 + (0.5 * h)) Green
    , Object (Rectangle pipeWidth (500 - floor h - pipeGap)) (x, negate (0.5 * (500 - h - fromIntegral pipeGap))) Green
    ]

instance ToObject Game where
  type FunctorType Game = []
  toObject :: Game -> [Object]
  toObject (Game cube pipe) = toObject pipe ++ (pure . runIdentity) (toObject cube) 

class Functor f => ToScene f where
  toScene :: f Object -> Scene

instance ToScene [] where
  toScene :: [Object] -> Scene
  toScene = initScene

instance ToScene Identity where
  toScene :: Identity Object -> Scene
  toScene (Identity obj) = initScene (pure obj)

initScene :: [Object] -> Scene
initScene = Scene [Object (Rectangle 200 500) (100, -250) BabyBlue, Object (Rectangle 200 100) (100, -550) Brown]


---------------------
--- SDL RENDERING ---
---------------------

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 windowW windowH}

drawObject :: Renderer -> Object -> IO ()
drawObject renderer object = do
  setDrawColor renderer (_color object)
  SDL.fillRect renderer (Just $ objToSDLRect object)
  
objToSDLRect :: Num a => Object -> SDL.Rectangle a
objToSDLRect obj =
  let (xC, yC) = negate <$> _pos obj
      (w, h) = (fromIntegral . _rectW &&& fromIntegral . _rectH) . _shape $ obj
      (xL, yL) = both (fromIntegral . floor) (xC - (w / 2), yC - (h / 2))
  in SDL.Rectangle (P (fromIntegral . floor <$> V2 xL yL)) (fromIntegral . floor <$> V2 w h)

centeredToLowerLeft :: Object -> Object
centeredToLowerLeft (Object (Rectangle w h) (xC, yC) color) =
  let xL = xC - fromIntegral (w `div` 2)
      yL = yC - fromIntegral (h `div` 2)
  in Object (Rectangle w h) (xL, yL) color

clearFrame :: Renderer -> IO ()
clearFrame renderer = do
  setDrawColor renderer White
  SDL.clear renderer

initSDL :: IO Renderer
initSDL = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" window
  SDL.createRenderer window (-1) SDL.defaultRenderer

setDrawColor :: Renderer -> Color -> IO ()
setDrawColor renderer color =
  case color of
    Red   -> SDL.rendererDrawColor renderer $= V4 255 0 0 0
    Blue  -> SDL.rendererDrawColor renderer $= V4 0 0 255 0
    BabyBlue  -> SDL.rendererDrawColor renderer $= V4 0 235 255 0
    Green -> SDL.rendererDrawColor renderer $= V4 120 200 15 0
    White -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
    Brown -> SDL.rendererDrawColor renderer $= V4 150 90 25 0
    Yellow -> SDL.rendererDrawColor renderer $= V4 255 200 50 0
    

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = setDrawColor renderer color >> SDL.clear renderer

draw :: Renderer -> Scene -> IO ()
draw renderer (Scene bg fg) = do
  clearFrame renderer
  traverse_ (drawObject renderer) (bg ++ fg)
  SDL.present renderer


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
data Cube = Cube { _y :: Double, _v :: Double }
data Game = Game { gameCube :: Cube, gamePipe :: Pipe }

windowW :: Num a => a
windowW = 200

windowH :: Num a => a
windowH = 600

cubeSize :: Int
cubeSize = 25

cubeX :: Double
cubeX = 50

pipeWidth :: Int
pipeWidth = 40

pipeHeight :: Double
pipeHeight = 100

pipeGap :: Int
pipeGap = 200

initGame :: Game
initGame = Game initCube initPipe

initCube :: Cube
initCube = Cube (-50) (-10)

initPipe :: Pipe
initPipe = Pipe 200 100


-----------------
-- GAME LOGIC ---
-----------------

checkCollision :: Game -> Bool
checkCollision (Game cube pipe) = 
  let cubeSize' = fromIntegral cubeSize
      xIntersects = _x pipe <= cubeSize' + 50 && _x pipe > 0
      yIntersects = _y cube <= negate (500 - _h pipe - cubeSize') || _y cube >= negate 500 + _h pipe + fromIntegral pipeGap
      groundIntersect = _y cube <= negate 500 + cubeSize'
  in groundIntersect || (xIntersects && yIntersects)

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
pipeHeightGen = noiseR (20, 500 - fromIntegral pipeGap - 20) (mkStdGen 3)

movingPipe :: Pipe -> SF Double Pipe
movingPipe (Pipe x0 h0) = switch sf cont
  where
    sf = proc h -> do
      x <- imIntegral x0 -< - 20
      respawn <- edge -< x < negate 100
      returnA -< (Pipe x h0, respawn `tag` h)
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
  renderer <- initSDL
  reactimate (return NoEvent) produceInput (handleOutput renderer) pipeline
    where
      produceInput :: Bool -> IO (DTime, Maybe (Event SDL.EventPayload))
      produceInput _ = do
        threadDelay 30000
        mevent <- SDL.pollEvent
        case mevent of
          Just event -> return (0.1, Just . Event $ SDL.eventPayload event)
          Nothing -> return (0.1, Nothing)
      handleOutput :: Renderer -> p -> Game -> IO Bool
      handleOutput r _ g@(Game c@(Cube y v) p) = do
        --putStrLn ("pos: " ++ show v ++ " vel: " ++ show v)
        --print $ toObject p
        liftIO $ draw r (toScene $ toObject g )
        return False
      pipeline :: SF (Event SDL.EventPayload) Game
      pipeline = parseSDLInput >>> game
