{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import SDL hiding (Event) --sdl2
--import SDL.Init
--import SDL.Video
import qualified SDL.Event as SDL
import Data.Text (pack, unpack)
import Linear (V2(..), V4(..))
import Linear.Affine (Point(P))
import Control.Monad
import Control.Monad.Loops --monad-loops
import Foreign.C.Types
import Data.Maybe (fromJust)

import FRP.Yampa as FRP --Yampa
import Data.IORef
import Data.Time.Clock --time

data GameData = GD {keys :: KeysData, x :: CInt, y :: CInt}
data KeysData = KD {up :: Bool, down :: Bool, left :: Bool, right :: Bool}
{-
toggleUp (KD up down left right) = KD (not up) down left right
toggleDown (KD up down left right) = KD up (not down) left right
toggleLeft (KD up down left right) = KD up down (not left) right
toggleRight (KD up down left right) = KD up down left (not right)
setUp (KD up down left right) = KD (True) down left right
setDown (KD up down left right) = KD up (True) left right
setLeft (KD up down left right) = KD up down (True) right
setRight (KD up down left right) = KD up down left (True)
-}

main :: IO ()
main = do
  renderer <- initializeRenderer
  timeRef <- getCurrentTime >>= newIORef
  
  -- gameDataRef 
  let startGameData = GD {keys=KD {up=False, down=False, Main.right=False, Main.left=False}, x=400, y=400}
  --appLoop renderer (400, 400)
  reactimate (initRenderer renderer >> return {-(GD {keys=KD {up=False, down=False, Main.right=False, Main.left=False}, x=400, y=400})-} []) (sense timeRef) actuation (perform startGameData)
  
sense :: IORef UTCTime -> Bool -> IO (Double, Maybe SDL.Event)
sense timeRef _ = do
  event <- SDL.pollEvent -- Get all pending events
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  
  writeIORef timeRef now
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, event)

actuation::Bool -> GameData -> IO Bool
actuation = undefined

fps = 60
perform::GameData -> SF SDL.Event GameData
perform startGamedata = let (xP, yP) = ((x startGamedata), (y startGamedata)) in proc input -> do
    newPos <- repeatedly (1.0/fps) () >>> arr (\e->case e of Event e -> updatePosition keys startGamedata xP yP ; NoEvent -> (xP, yP)) -< ()
    let newKeys = updateKeys input startGamedata
    returnA (arr (\(kd, xy) -> GD {keys = kd, x = fst xy, y = snd xy})) -< (newKeys, newPos)

updateKeys = undefined
updatePosition = undefined

moveCursor:: SF (SDL.Event, GameData) GameData
moveCursor = proc (events, gamedata) -> do
    returnA -< gamedata

initializeRenderer = do 
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  initRenderer renderer
  return renderer


initRenderer renderer = do
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer

{-
appLoop :: Renderer -> (CInt, CInt) -> IO ()
appLoop renderer coords = do
  events <- pollEvents
  
  let keyboardPressEvents = filter (\event -> case eventPayload event of KeyboardEvent e -> keyboardEventKeyMotion e == Pressed; otherwise -> False) events
  let keyPressed keycode = any (\event -> case eventPayload event of KeyboardEvent e -> keysymKeycode (keyboardEventKeysym e) == keycode) keyboardPressEvents
  let qPressed = keyPressed KeycodeQ
  let rightPressed = keyPressed KeycodeRight
      leftPressed = keyPressed KeycodeLeft
      upPressed = keyPressed KeycodeUp
      downPressed = keyPressed KeycodeDown
  let oldX = fst coords
      oldY = snd coords
  let newY = if upPressed
      then max 0 (oldY - 1)
      else if downPressed
      then min 800 (oldY + 1)
      else oldY
  let newX = if leftPressed
      then max 0 (oldX - 1)
      else if rightPressed
      then min 800 (oldX + 1)
      else oldX
  rendererDrawColor renderer $= V4 255 0 0 255
  drawPoint renderer (P (V2 newX newY))
  present renderer
  if qPressed 
     then putStrLn "Q pressed."
     else appLoop renderer (newX, newY)
-}
