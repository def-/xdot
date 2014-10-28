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

import Graphics.XDot.Types hiding (w, h, filled, alignment, text, name, size)

import Graphics.UI.Gtk (PangoRectangle(..), layoutSetFontDescription,
  layoutGetExtents, layoutContextChanged, fontDescriptionFromString,
  fontDescriptionSetSize, showLayout, cairoContextSetFontOptions,
  cairoContextGetFontOptions, layoutGetContext, createLayout)
import Graphics.Rendering.Cairo hiding (x, y)

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
     Object t -- ^ id of the node to highlight
  -> Rectangle -- ^ dimensions of the graph, as returned by 'Graphics.XDot.Parser.getSize'
  -> [(Object t, Operation)] -- ^ operations, as returned by 'Graphics.XDot.Parser.getOperations'
  -> Render [(Object t, Rectangle)] -- ^ dimensions of the rendered nodes on the screen
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

stylizedDraw :: Eq t => Bool -> Object t -> Object t -> Render a -> DrawState ()
stylizedDraw filled mn hover renderOps = do
  (rf,gf,bf,af) <- gets filledColor
  (rs,gs,bs,as) <- gets strokeColor
  lWidth <- gets lineWidth
  lStyle <- gets lineStyle

  lift $ do
    when filled $ do
      if mn /= None && mn == hover
        then setSourceRGBA 1 0.8 0.8 1
        else setSourceRGBA rf gf bf af

      save
      renderOps
      restore

      fillPreserve
      fill

    setLineWidth lWidth
    setDash lStyle 0

    if mn /= None && mn == hover
      then setSourceRGBA 1 0 0 1
      else setSourceRGBA rs gs bs as

    save
    renderOps
    restore

    stroke

draw :: Eq t => Object t -> (Object t, Operation) -> DrawState [(Object t, Rectangle)]
draw hover (mn, Ellipse (x,y) w h filled) = do
  stylizedDraw filled hover mn $ do
    translate x y
    scale w h
    moveTo 1 0
    arc 0 0 1 0 (2 * pi)

  return $ case mn of
    None -> []
    o -> [(o, (x - w, y + h, 2 * w, 2 * h))]

draw hover (mn, Polygon a@((x,y):xys) filled) = do
  stylizedDraw filled hover mn $ do
    moveTo x y
    mapM_ (uncurry lineTo) xys
    closePath

  let xs = x : map fst a
  let ys = y : map snd a

  return $ case mn of
    None -> []
    o -> [(o, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]

draw _ (_, Polygon [] _) = return []

draw hover (mn, Polyline a@((x,y):xys)) = do
  stylizedDraw False hover mn $ do
    moveTo x y
    mapM_ (uncurry lineTo) xys

  let xs = x : map fst a
  let ys = y : map snd a

  return $ case mn of
    None -> []
    o -> [(o, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]

draw _ (_, Polyline []) = return []

draw hover (mn, BSpline ((x,y):xys) filled) = do
  stylizedDraw filled hover mn $ do
    moveTo x y
    drawBezier xys

  return $ case mn of
    None -> []
    o -> [ (o, (x  - 15, y  + 15, 30, 30))
         , (o, (xe - 15, ye + 15, 30, 30))
         ]

  where drawBezier ((x1,y1):(x2,y2):(x3,y3):xys2) = do
          curveTo x1 y1 x2 y2 x3 y3
          drawBezier xys2
        drawBezier _ = return ()
        (xe,ye) = last xys

draw _ (_, BSpline [] _) = return []

draw hover (mn, Text (x,y) alignment w text) = do
  fontName' <- gets fontName
  fontSize' <- gets fontSize

  layout <- lift $ createLayout text
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

  (_, PangoRectangle _ _ w2 h2) <- liftIO $ layoutGetExtents layout

  let (f, w3, h3, descent) = if w2 > w
        then (w / w2, w,  h2 * w / w2, 4 * w / w2)
        else (1,      w2, h2,          4)

  let x3 = case alignment of
             LeftAlign   -> x
             CenterAlign -> x - 0.5 * w3
             RightAlign  -> x -       w3
      y3 = y + h3 - descent

  stylizedDraw False hover mn $ do
    moveTo x3 y3
    scale f (-f)
    showLayout layout

  return $ case mn of
    None -> []
    o -> [(o, (x3, y3, w3, h3))]

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

draw _ (_, FontCharacteristics{}) = return [] -- TODO
