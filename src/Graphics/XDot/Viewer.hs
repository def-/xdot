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

import Graphics.XDot.Types hiding (w, h, filled, alignment, text, name)

import Graphics.UI.Gtk hiding (Color, Rectangle, descent)
import Graphics.Rendering.Cairo

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
  boundingBoxes <- mapM (draw hover) ops
  restore
  return
    $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w,h)))
    $ concat boundingBoxes

draw :: Eq t => Maybe t -> (Maybe t, Operation) -> Render [(t, Rectangle)]

draw hover (mn, Ellipse (x,y) w h filled) = do
  save
  translate x y
  scale w h
  moveTo 1 0
  arc 0 0 1 0 (2 * pi)
  restore

  if isJust mn && mn == hover
    then setSourceRGB 1 0 0
    else setSourceRGB 0 0 0

  setLineWidth 1
  if filled then fill else stroke

  setSourceRGB 0 0 0

  return $ case mn of
    Just node -> [(node, (x - w, y + h, 2 * w, 2 * h))]
    Nothing   -> []

draw _ (_, Polygon ((x,y):xys) filled) = do
  moveTo x y
  _ <- mapM (uncurry lineTo) xys
  closePath
  fillStroke filled

draw _ (_, Polygon [] _) = return []

draw _ (_, Polyline _) = return []

draw _ (_, BSpline ((x,y):xys) filled) = do
  moveTo x y
  drawBezier xys
  fillStroke filled

  where drawBezier ((x1,y1):(x2,y2):(x3,y3):xys2) = do
          curveTo x1 y1 x2 y2 x3 y3
          drawBezier xys2
        drawBezier _ = return ()

draw _ (_, BSpline [] _) = return []

draw _ (_, Text (x,y) alignment w text) = do
  layout <- createLayout "text"
  context <- liftIO $ layoutGetContext layout

  fo <- liftIO $ cairoContextGetFontOptions context

  fontOptionsSetAntialias fo AntialiasDefault
  fontOptionsSetHintStyle fo HintStyleNone
  fontOptionsSetHintMetrics fo HintMetricsOff
  liftIO $ cairoContextSetFontOptions context fo

  liftIO $ layoutContextChanged layout

  font <- liftIO fontDescriptionNew
  liftIO $ fontDescriptionSetFamily font "Times-Roman"
  liftIO $ fontDescriptionSetSize font 14
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

draw _ (_, Color (r,g,b,a) _) = do
  setSourceRGBA r g b a
  return []

-- TODO: Implement Font, Style, Image
draw _ (_, Font _ _) = return []
draw _ (_, Style _) = return []
draw _ (_, Image{}) = return []

fillStroke :: Bool -> Render [a]
fillStroke filled = do
  setLineWidth 1
  if filled then fillPreserve >> fill else stroke
  return []
