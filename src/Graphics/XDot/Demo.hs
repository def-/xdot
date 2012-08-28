-- Example of an XDot drawing with clickable nodes

import System.Environment
import Control.Monad
import Data.IORef

import Data.Text.IO hiding (putStrLn)
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy as B

import Data.GraphViz

import Graphics.XDot.Parser
import Graphics.XDot.Viewer
import Graphics.XDot.Types hiding (w, h)

import Graphics.UI.Gtk hiding (Box, Signal, Dot, Point, Rectangle)
import Graphics.Rendering.Cairo
import qualified Graphics.UI.Gtk.Gdk.Events as E

data State = State
  { objects  :: ([(Maybe Int, Operation)], Rectangle)
  , bounds   :: [(Int, Rectangle)]
  , mousePos :: Point
  , hover    :: Maybe Int
  }

main = getArgs >>= run

run args = do
  dotText <- L.readFile $ head args
  let dg = parseDotGraph dotText :: DotGraph Int
  xDotText <- graphvizWithHandle Dot dg XDot hGetContents
  let xdg = fromCanonical (parseDotGraph $ B.fromChunks [xDotText] :: DotGraph Int)
  let objects = (getOperations xdg, getSize xdg)

  state <- newIORef $ State objects [] (0,0) Nothing

  initGUI
  window <- windowNew
  canvas <- drawingAreaNew

  set window [ windowTitle := "XDot Demo"
             , containerChild := canvas
             ]

  onExpose canvas $ const $ do
    redraw canvas state
    return True

  onMotionNotify canvas False $ \e -> do
    modifyIORef state (\s -> s {mousePos = (E.eventX e, E.eventY e)})
    tick canvas state
    return True

  onButtonPress canvas $ \e -> do
    when (E.eventButton e == LeftButton && E.eventClick e == SingleClick) $
      click canvas state dg
    return True

  widgetShowAll window
  onDestroy window mainQuit
  mainGUI

click canvas state dg = do
  s <- readIORef state

  case hover s of
    Just t ->
      putStrLn $ (show (nodeStmts $ graphStatements dg) !! t) ++ " clicked"
    _ -> return ()

tick canvas state = do
  s <- readIORef state
  let oldHover = hover s

  modifyIORef state $ \s -> (
    let (mx, my) = mousePos s
        check (name, (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then Just name else Nothing
    in s {hover = msum $ map check (bounds s)}
    )

  s <- readIORef state
  unless (oldHover == hover s) $ widgetQueueDraw canvas

redraw canvas state = do
  s <- readIORef state
  E.Rectangle rx ry rw rh <- widgetGetAllocation canvas

  let (ops, size@(sx,sy,sw,sh)) = objects s

  boundingBoxes <- render canvas $ do
    -- Proportional scaling
    let scalex = min (fromIntegral rw / sw) (fromIntegral rh / sh)
        scaley = scalex
        offsetx = 0.5 * fromIntegral rw
        offsety = 0.5 * fromIntegral rh
    save
    translate offsetx offsety
    scale scalex scaley

    result <- drawAll (hover s) size ops

    restore
    return $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w*scalex,h*scaley))) result

  modifyIORef state (\s -> s {bounds = boundingBoxes})

render canvas r = do
    win <- widgetGetDrawWindow canvas
    renderWithDrawable win r
