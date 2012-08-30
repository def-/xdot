{- |
   Module      : Graphics.XDot.Viewer
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
  { fontName    :: String
  , fontSize    :: Double
  , lineWidth   :: Double
  , lineStyle   :: [Double]
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

  boundingBoxes <- evalStateT (mapM (draw hover) ops) $ DState "" 1 1 [] (1,1,1,1) (0,0,0,1)

  restore
  return
    $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w,h)))
    $ concat boundingBoxes

stylizedDraw :: Eq t => Bool -> Maybe t -> Maybe t -> Render a -> DrawState ()
stylizedDraw filled mn hover renderOps = do
  (r,g,b,a) <- getCorrectColor filled
  lWidth <- gets lineWidth
  lStyle <- gets lineStyle

  lift $ do
    if isJust mn && mn == hover
      then setSourceRGBA 1 0 0 1
      else setSourceRGBA r g b a
    setLineWidth lWidth
    setDash lStyle 0

    save
    renderOps
    restore

    if filled then fillPreserve >> fill else stroke

draw :: Eq t => Maybe t -> (Maybe t, Operation) -> DrawState [(t, Rectangle)]
draw hover (mn, Ellipse (x,y) w h filled) = do
  stylizedDraw filled hover mn $ do
    translate x y
    scale w h
    moveTo 1 0
    arc 0 0 1 0 (2 * pi)

  return $ case mn of
    Just node -> [(node, (x - w, y + h, 2 * w, 2 * h))]
    Nothing   -> []

draw hover (mn, Polygon ((x,y):xys) filled) = do
  stylizedDraw filled hover mn $ do
    moveTo x y
    mapM_ (uncurry lineTo) xys
    closePath

  let xs = x : map fst xys
  let ys = y : map snd xys

  return $ case mn of
    Just node -> [(node, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]
    Nothing   -> []

draw _ (_, Polygon [] _) = return []

draw _ (_, Polyline _) = return []

draw hover (mn, BSpline ((x,y):xys) filled) = do
  stylizedDraw filled hover mn $ do
    moveTo x y
    drawBezier xys

  return []

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

    -- This does not work with "Times Roman", but it works with a font that is
    -- installed on the system
    --font <- liftIO fontDescriptionNew
    --liftIO $ fontDescriptionSetFamily font "Nimbus Roman No9 L, Regular"
    --liftIO $ fontDescriptionSetFamily font "Times Roman"
    --liftIO $ fontDescriptionSetSize font fontSize'

    -- Only fontDescriptionFromString works as expected, choosing a similar
    -- alternative font when the selected one is not available
    font <- liftIO $ fontDescriptionFromString fontName'
    liftIO $ fontDescriptionSetSize font fontSize'
    liftIO $ layoutSetFontDescription layout (Just font)

    liftIO $ layoutSetText layout text

    (_, PangoRectangle _ _ w2 h2) <- liftIO $ layoutGetExtents layout

    let (f, w3, h3, descent) = if w2 > w
          then (w / w2, w,  h2 * w / w2, 4 * w / w2)
          else (1,      w2, h2,          4)

    let x3 = case alignment of
               LeftAlign   -> x
               CenterAlign -> x - 0.5 * w3
               RightAlign  -> x -       w3
        y3 = y + h3 - descent

    moveTo x3 y3
    save
    scale f (-f)

    showLayout layout

    restore

    return []

draw _ (_, Color color filled) = do
  modify (\s -> if filled
    then s{filledColor = color}
    else s{strokeColor = color})
  return []

draw _ (_, Font size name) = do
  modify (\s -> s{fontName = fixedName, fontSize = size})
  return []

  -- Pango does not like "Times-Roman", but works with "Times Roman".
  -- Graphviz handles this in plugin/pango/gvtextlayout_pango.c
  where fixedName = map fixName name
        fixName '-' = ' '
        fixName x   = x

draw _ (_, Style x) = do
  case x of -- TODO: Some styles missing
    "solid"  -> modify (\s -> s{lineStyle = []}) -- always on
    "dashed" -> modify (\s -> s{lineStyle = [6,6]}) -- 6 pts on, 6 pts off
    "dotted" -> modify (\s -> s{lineStyle = [2,4]}) -- 2 pts on, 4 pts off
    _ -> return ()
  return []

draw _ (_, Image{}) = return [] -- TODO

getCorrectColor :: Bool -> DrawState RGBA
getCorrectColor filled = gets $ if filled then filledColor else strokeColor
