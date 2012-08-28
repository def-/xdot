{-# LANGUAGE OverloadedStrings #-}

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
module Graphics.XDot.Parser (
  getOperations,
  getSize
)
where

import Control.Monad
import Data.Maybe
import Data.Char
import Data.Ratio

import qualified Data.Foldable as F
import qualified Data.Text.Lazy as B
import qualified Data.Text.Lazy.Read as B
import qualified Text.ParserCombinators.Poly.StateText as P

import Data.GraphViz.Types hiding (parse, attrs)
import Data.GraphViz.Parsing hiding (parse)
import qualified Data.GraphViz.Attributes.Complete as A
import qualified Data.GraphViz.Types.Generalised as G

import Graphics.XDot.Types hiding (w, h, filled, baseline, width, alignment, size, text, xy, name)

-- | Extract all operations of an xdot graph and connect them to the node they
--   belong to, if any.
getOperations :: G.DotGraph a -> [(Maybe a, Operation)]
getOperations (G.DotGraph _ _ _ graphStatements) = F.foldr handle [] graphStatements
  where handle (G.GA (GraphAttrs attrs)) l = zip (repeat Nothing) (foldr handleInternal [] attrs) ++ l
        handle (G.DN (DotNode ident attrs)) l = zip (repeat $ Just ident) (foldr handleInternal [] attrs) ++ l
        handle (G.DE (DotEdge _ _ attrs)) l = zip (repeat Nothing) (foldr handleInternal [] attrs) ++ l
        handle _ l = l

        handleInternal (A.UnknownAttribute "_draw_" r) l = parse r ++ l
        handleInternal (A.UnknownAttribute "_ldraw_" r) l = parse r ++ l
        handleInternal (A.UnknownAttribute "_hdraw_" r) l = parse r ++ l
        handleInternal (A.UnknownAttribute "_tdraw_" r) l = parse r ++ l
        handleInternal (A.UnknownAttribute "_hldraw_" r) l = parse r ++ l
        handleInternal (A.UnknownAttribute "_tlldraw_" r) l = parse r ++ l
        handleInternal _ l = l

-- | Extract the dimensions of the graph when drawn.
getSize :: G.DotGraph a -> Rectangle
getSize (G.DotGraph _ _ _ graphStatements) = F.foldr handle (0,0,0,0) graphStatements
  where handle (G.GA (GraphAttrs attrs)) _ = foldr handleInternal (0,0,0,0) attrs
        handle _ l = l

        handleInternal (A.BoundingBox (A.Rect (A.Point x y _ _) (A.Point w h _ _))) _ = (x,y,w,h)
        handleInternal _ l = l

parse :: B.Text -> [Operation]
parse = Data.GraphViz.Parsing.runParser' $ P.many $ do
  t <- P.next
  character ' '

  case t of
    'E' -> parseEllipse True
    'e' -> parseEllipse False
    'P' -> parsePolygon True
    'p' -> parsePolygon False
    'L' -> parsePolyline
    'B' -> parseBSpline False
    'b' -> parseBSpline True
    'T' -> parseText
    'C' -> parseColor True
    'c' -> parseColor False
    'F' -> parseFont
    'S' -> parseStyle
    'I' -> parseImage
    _   -> fail "Unknown Operation"

  where
    parseEllipse filled = do
      p <- parsePoint
      (w,h) <- parsePoint
      return $ Ellipse p w h filled

    parsePolygon filled = do
      xs <- parsePoints
      return $ Polygon xs filled

    parsePolyline = liftM Polyline parsePoints

    parseBSpline filled = do
      xs <- parsePoints
      return $ BSpline xs filled

    parseText = do
      baseline <- parsePoint
      j <- parseInt'
      let alignment = case j of
                        -1 -> LeftAlign
                        0  -> CenterAlign
                        1  -> RightAlign
                        _  -> error "Unexpected alignment"
      character ' '
      width <- parseFloat'
      character ' '
      text <- parseString
      return $ Text baseline alignment width text

    parseFont = do
      size <- parseFloat'
      character ' '
      name <- parseString
      return $ Font size name

    parseStyle = liftM Style parseString

    parseImage = do
      xy <- parsePoint
      (w,h) <- parsePoint
      name <- parseString
      return $ Image xy w h name

    parseString = do
      n <- parseInt
      character ' '
      character '-'
      text <- replicateM (fromInteger n) P.next
      character ' '
      return text

    parsePoints = do
      n <- parseInt
      character ' '
      replicateM (fromInteger n) parsePoint

    parsePoint = do
      x <- parseFloat'
      character ' '
      y <- parseFloat'
      character ' '
      return (x,y)

    parseColor filled = do -- TODO: Not complete
      _ <- parseInt
      character ' '
      character '-'
      character '#'
      r <- parseHex
      g <- parseHex
      b <- parseHex
      a <- parseHex
      character ' '
      return $ Color (r,g,b,a) filled
     where parseHex = liftM hexToFloat $ replicateM 2 P.next
           hexToFloat s = foldl (\x y -> 16 * x + fromIntegral (digitToInt y)) 0 s / 255

    -- The following functions are taken from GraphViz/Parsing.hs, as they are not
    -- exported.

    parseSigned p = (character '-' >> liftM negate p)
                    `P.onFail`
                    p

    parseInt = do cs <- P.many1Satisfy isDigit
                  case B.decimal cs of
                    Right (n,"")  -> return n
                    Right (_,txt) -> fail $ "Trailing digits not parsed as Integral: " ++ B.unpack txt
                    Left err      -> fail $ "Could not read Integral: " ++ err
               `P.adjustErr` ("Expected one or more digits\n\t"++)

    parseInt' = parseSigned parseInt

    parseFloat = do ds   <- P.manySatisfy isDigit
                    frac <- P.optional
                            $ do character '.'
                                 P.manySatisfy isDigit
                    when (B.null ds && noDec frac)
                      (fail "No actual digits in floating point number!")
                    expn  <- P.optional parseExp
                    when (isNothing frac && isNothing expn)
                      (fail "This is an integer, not a floating point number!")
                    let frac' = fromMaybe "" frac
                        expn' = fromMaybe 0 expn
                    ( return . fromRational . (* (10^^(expn' - fromIntegral (B.length frac'))))
                      . (%1) . Data.GraphViz.Parsing.runParser' parseInt) (ds `B.append` frac')
                 `P.onFail`
                 fail "Expected a floating point number"
      where
        parseExp = do character 'e'
                      (character '+' >> parseInt)
                       `P.onFail`
                       parseInt'
        noDec = maybe True B.null

    parseFloat' = parseSigned ( parseFloat
                                `onFail`
                                liftM fI parseInt
                              )
      where
        fI :: Integer -> Double
        fI = fromIntegral
