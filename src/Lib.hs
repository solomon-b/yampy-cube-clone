{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Lib where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.IORef
import Data.Foldable
import Data.Functor.Identity

import FRP.Yampa
import qualified SDL 
import SDL (Renderer, Point(..), V4(..), V2(..), WindowConfig(..), ($=))

----------------
--- GRAPHICS ---
----------------

data Color = Red | Green | Blue | BabyBlue | White | Brown
data Shape = Rectangle Int Int
data Object = Object
  { _shape :: Shape
  , _pos   :: (Double, Double)
  , _color :: Color
  }

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
  toObject (Cube y _) = Identity $ Object (Rectangle 25 25) (50, y) Green

instance ToObject Pipe where
  type FunctorType Pipe = []
  toObject :: Pipe -> [] Object
  toObject (Pipe x h) = [Object (Rectangle 40 (floor h)) (x, negate $ 500 - h) Brown, Object (Rectangle 40 (350 - floor h)) (x, 0) Brown]

instance ToObject Game where
  type FunctorType Game = Identity
  toObject :: Game -> Identity Object
  toObject (Game cube) = toObject cube

class Functor f => ToScene f where
  toScene :: f Object -> Scene

instance ToScene [] where
  toScene :: [Object] -> Scene
  toScene = initScene

instance ToScene Identity where
  toScene :: Identity Object -> Scene
  toScene (Identity obj) = initScene (pure obj)

initScene :: [Object] -> Scene
initScene = Scene $ [Object (Rectangle 200 500) (0, 0) BabyBlue, Object (Rectangle 200 100) (0, -500) Brown] ++ toObject (Pipe 150 100)


---------------------
--- SDL RENDERING ---
---------------------

window :: WindowConfig
window = SDL.defaultWindow { windowInitialSize = V2 200 600}

drawObject :: Renderer -> Object -> IO ()
drawObject renderer object = do
  setDrawColor renderer (_color object)
  case _shape object of
    Rectangle w h -> drawRect renderer w h (_pos object)
  return ()
  
drawRect :: Renderer -> Int -> Int -> (Double, Double) -> IO ()
drawRect renderer w h (x, y) = do
  let rect = SDL.Rectangle (P $ V2 (fromIntegral . floor $ x) (negate . fromIntegral . floor $ y)) (V2 (fromIntegral w) (fromIntegral h))
  SDL.fillRect renderer $ Just rect
  
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
    Green -> SDL.rendererDrawColor renderer $= V4 0 255 0 0
    White -> SDL.rendererDrawColor renderer $= V4 255 255 255 255
    Brown -> SDL.rendererDrawColor renderer $= V4 150 90 25 0
    

drawBackground :: Renderer -> Color -> IO ()
drawBackground renderer color = setDrawColor renderer color >> SDL.clear renderer

draw :: Renderer -> Scene -> IO ()
draw renderer (Scene bg fg) = do
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

  
-----------------
-- GAME LOGIC ---
-----------------

data Pipe = Pipe { _x :: Double, _h :: Double }
data Cube = Cube { _y :: Double, _v :: Double }
newtype Game = Game { gameCube :: Cube }

initGame :: Game
initGame = Game initCube

initCube :: Cube
initCube = Cube (-50) (-10)

game :: SF AppInput Game
game = proc input -> do
  cube <- flappingCube initCube -< input
  returnA -< Game cube

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
    cont (Cube y v) = flappingCube (Cube y (v + 100))

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
      handleOutput r _ (Game c@(Cube y v)) = do
        putStrLn ("pos: " ++ show v ++ " vel: " ++ show v)
        liftIO $ draw r (toScene $ toObject c)
        return False
      pipeline :: SF (Event SDL.EventPayload) Game
      pipeline = parseSDLInput >>> game
