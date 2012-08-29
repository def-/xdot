{- |
   Module      : Graphics.XDot.Parser
   Description : Parsing xdot graphs to get their operations
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

   After an xdot file has been opened using GraphViz, its drawing operations
   can be parsed using this module.

   > xDotText <- L.readFile "example.xdot"
   > let xDotGraph = parseDotGraph xDotText :: G.DotGraph Int
   > let operations = getOperations xDotGraph

   xdot files can be created using the dot binary from the Graphviz package:

   > $ cat example.dot
   > digraph {
   >     0 [label=""];
   >     1 [label=":"];
   >     0 -> 1 [label="[1..]"];
   > }
   > $ dot -Txdot example.dot > example.xdot

   Or you can skip saving an xdot file and use a dot file directly:

   > $ dotText <- L.readFile "example.dot"
   > $ let dotGraph = parseDotGraph dotText :: G.DotGraph Int
   > $ xDotText <- graphvizWithHandle Dot dotGraph XDot T.hGetContents
   > $ let xDotGraph = parseDotGraph $ B.fromChunks [xDotText] :: G.DotGraph Int
   > $ getOperations xDotGraph
   > [ (Nothing,Color {rgba = (1.0,1.0,1.0,1.0), filled = False})
   > , (Nothing,Color {rgba = (1.0,1.0,1.0,1.0), filled = True})
   > , (Nothing,Polygon {points = [(0.0,-1.0),(0.0,130.0),(55.0,130.0),(55.0,-1.0)], filled = True})
   > , (Just 0,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Just 0,Ellipse {xy = (27.0,112.0), w = 27.0, h = 18.0, filled = False})
   > , (Just 1,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Just 1,Ellipse {xy = (27.0,19.0), w = 27.0, h = 19.0, filled = False})
   > , (Just 1,Font {size = 14.0, name = "Times-Roman"})
   > , (Just 1,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Just 1,Text {baseline = (27.0,15.0), alignment = CenterAlign, width = 4.0, text = ":"})
   > , (Nothing,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Nothing,BSpline {points = [(27.0,94.0),(27.0,81.0),(27.0,63.0),(27.0,48.0)], filled = False})
   > , (Nothing,Style {style = "solid"})
   > , (Nothing,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Nothing,Color {rgba = (0.0,0.0,0.0,1.0), filled = True})
   > , (Nothing,Polygon {points = [(31.0,48.0),(27.0,38.0),(24.0,48.0)], filled = True})
   > , (Nothing,Font {size = 14.0, name = "Times-Roman"})
   > , (Nothing,Color {rgba = (0.0,0.0,0.0,1.0), filled = False})
   > , (Nothing,Text {baseline = (39.0,62.0), alignment = CenterAlign, width = 24.0, text = "[1..]"})
   > ]

   The following imports are needed for this:

   > import Data.GraphViz
   > import Data.Text.IO as T
   > import qualified Data.Text.Lazy as B
   > import qualified Data.Text.Lazy.IO as L
   > import qualified Data.GraphViz.Types.Generalised as G
 -}

{- |
   Module      : Graphics.XDot.Viewer
   Description : View xdot graphs by executing their operations
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

   This module draws the operations of an xdot graph using Cairo and Pango on a
   Gtk canvas.
 -}
module Graphics.XDot.Viewer (
  drawAll
)
where

import Data.Maybe

import Graphics.XDot.Types hiding (w, h, filled, alignment, text, name, size)

import Graphics.UI.Gtk hiding (Color, Rectangle, descent)
import Graphics.Rendering.Cairo

import Control.Monad.State hiding (State)
import qualified Control.Monad.State as MS

type RGBA = (Double, Double, Double, Double)

data DState = DState
  { fontName :: String
  , fontSize :: Double
  , filledColor :: RGBA
  , strokeColor :: RGBA
  }
type DrawState a = MS.StateT DState Render a

-- | Draw an xdot graph, possibly highlighting a node.
drawAll :: Eq t => 
     Maybe t -- ^ id of the node to highlight
  -> Rectangle -- ^ dimensions of the graph, as returned by 'Graphics.XDot.Parser.getSize'
  -> [(Maybe t, Operation)] -- ^ operations, as returned by 'Graphics.XDot.Parser.getOperations'
  -> Render [(t, Rectangle)] -- ^ dimensions of the rendered nodes on the screen
drawAll hover (_,_,sw,sh) ops = do
  let scalex = 1
      scaley = -1
      offsetx = -0.5 * sw
      offsety = 0.5 * sh
  save
  translate offsetx offsety
  scale scalex scaley

  boundingBoxes <- evalStateT (mapM (draw hover) ops) $ DState "" 1 (0,0,0,1) (0,0,0,1)

  restore
  return
    $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w,h)))
    $ concat boundingBoxes

draw :: Eq t => Maybe t -> (Maybe t, Operation) -> DrawState [(t, Rectangle)]
draw hover (mn, Ellipse (x,y) w h filled) = do
  (r,g,b,a) <- getCorrectColor filled

  lift $ do
    if isJust mn && mn == hover
      then setSourceRGBA 1 0 0 1
      else setSourceRGBA r g b a
    fillStroke filled

    save
    translate x y
    scale w h
    moveTo 1 0
    arc 0 0 1 0 (2 * pi)
    restore

    return $ case mn of
      Just node -> [(node, (x - w, y + h, 2 * w, 2 * h))]
      Nothing   -> []

draw hover (mn, Polygon ((x,y):xys) filled) = do
  (r,g,b,a) <- getCorrectColor filled

  lift $ do
    if isJust mn && mn == hover
      then setSourceRGBA 1 0 0 1
      else setSourceRGBA r g b a
    fillStroke filled

    moveTo x y
    mapM (uncurry lineTo) xys
    closePath

    let xs = x : map fst xys
    let ys = y : map snd xys

    return $ case mn of
      Just node -> [(node, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]
      Nothing   -> []

draw _ (_, Polygon [] _) = return []

draw _ (_, Polyline _) = return []

draw _ (_, BSpline ((x,y):xys) filled) = do
  (r,g,b,a) <- getCorrectColor filled

  lift $ do
    setSourceRGBA r g b a

    moveTo x y
    drawBezier xys
    fillStroke filled

    where drawBezier ((x1,y1):(x2,y2):(x3,y3):xys2) = do
            curveTo x1 y1 x2 y2 x3 y3
            drawBezier xys2
          drawBezier _ = return ()

draw _ (_, BSpline [] _) = return []

draw _ (_, Text (x,y) alignment w text) = do
  (r,g,b,a) <- getCorrectColor False -- stroke, not filled
  fontName' <- gets fontName
  fontSize' <- gets fontSize

  lift $ do
    setSourceRGBA r g b a

    layout <- createLayout "text"
    context <- liftIO $ layoutGetContext layout

    fo <- liftIO $ cairoContextGetFontOptions context

    fontOptionsSetAntialias fo AntialiasDefault
    fontOptionsSetHintStyle fo HintStyleNone
    fontOptionsSetHintMetrics fo HintMetricsOff
    liftIO $ cairoContextSetFontOptions context fo

    liftIO $ layoutContextChanged layout

    font <- liftIO fontDescriptionNew
    liftIO $ fontDescriptionSetFamily font fontName'
    liftIO $ fontDescriptionSetSize font fontSize'
    liftIO $ layoutSetFontDescription layout (Just font)

    liftIO $ layoutSetText layout text

    (_, PangoRectangle _ _ w2 h2) <- liftIO $ layoutGetExtents layout

    let (f, w3, h3, descent) = if w2 > w
          then (w / w2, w,  h2 * w / w2, 2 * w / w2)
          else (1,      w2, h2,          2)

    let x3 = case alignment of
               LeftAlign   -> x
               CenterAlign -> x - 0.5 * w3
               RightAlign  -> x -       w3
        y3 = y + h3 - descent

    moveTo x3 y3
    save
    scale f (-f)

    showLayout layout

    return ()

    restore
    return []

draw _ (_, Color color@(r,g,b,a) filled) = do
  --setCorrectColor filled color
  lift $ setSourceRGBA r g b a
  return []

-- TODO: Implement Font, Style, Image
draw _ (_, Font size name) = do
  modify (\(DState _ _ a b) -> DState name size a b)
  return []
draw _ (_, Style x) = do
  return []
draw _ (_, Image{}) = return []

getCorrectColor :: Bool -> DrawState RGBA
getCorrectColor filled = gets $ if filled then filledColor else strokeColor

setCorrectColor :: Bool -> RGBA -> DrawState ()
setCorrectColor filled color = modify (\(DState a b c d) -> if filled then DState a b color d else DState a b c color)

fillStroke :: Bool -> Render [a]
fillStroke filled = do
  setLineWidth 1
  if filled then fillPreserve >> fill else stroke
  return []
