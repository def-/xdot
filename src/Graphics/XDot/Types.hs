{- |
   Module      : Graphics.XDot.Types
   Description : Representations of xdot parameters.
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

   This module contains various types used to represent xdot parameters.
 -}
module Graphics.XDot.Types (
  Point,
  Rectangle,
  Alignment(..),
  Operation(..)
  )
  where

-- | A simple point, consisting of an x and y position.
type Point = (Double, Double)

-- | A rectangle, x and y position, width and height.
type Rectangle = (Double, Double, Double, Double)

-- | Alignment of text.
data Alignment = LeftAlign
               | CenterAlign
               | RightAlign
               deriving Show

-- | Drawing operations supported by xdot. See
--   <http://www.graphviz.org/doc/info/output.html#d:xdot> for more information

data Operation = Ellipse { xy :: Point, w :: Double, h :: Double, filled :: Bool }
               | Polygon { points :: [Point], filled :: Bool }
               | Polyline { points :: [Point] }
               | BSpline { points :: [Point], filled :: Bool }
               | Text { baseline :: Point, alignment :: Alignment, width :: Double, text :: String }
               | Color { rgba :: (Double, Double, Double, Double), filled :: Bool }
               | Font { size :: Double, name :: String }
               | Style { style :: String }
               | Image { xy :: Point, w :: Double, h :: Double, name :: String }
               deriving Show
