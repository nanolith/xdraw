Shapes
======

This module contains various shape drawing functions that are used to create
diagrams.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Shapes (
>   roundedBox, squareBox, diamondBox, floatingLabel, dashedFence, topFence
> ) where

We use the `Diagrams` package.

> import Diagrams.Prelude
> import Diagrams.Backend.Postscript
> import Diagrams.TwoD.Text

We use `Data.Maybe` for dealing with `Maybe` types.

> import Data.Maybe

The first method we need is a `placeObject` method which will place an object at
the given offset within a diagram.  This is a bit of a hack to give us somewhat
absolute coordinates in a relative coordinate system like `Diagrams`, but it
works for our purposes.

> placeObject :: Diagram B -> (Double, Double) -> Diagram B
> placeObject obj point =
>       flip atPoints (repeat obj) $ map p2 $ [point]

The `maybeNamed` method takes a `Maybe String` value.  If this value is set,
then it names the given object that name.  If this value is not set, then it
does nothing.

> maybeNamed :: Maybe String -> Diagram B -> Diagram B
> maybeNamed name obj =
>       nameFun obj
>   where
>       nameFun = fromMaybe id (named <$> name)

The `labeledShapeWithShadow` method creates a labeled shape with a shadow.  Two
string parameters are required.  The first is the label for the shape, and the
second is the name to give to this shape for addressing during path
calculations.  We offset the shadow by a quarter unit so that the shadow appears
on the bottom right side of the image.

> labeledShapeWithShadow :: Diagram B -> String -> Maybe String
>                           -> (Double, Double) -> Diagram B
> labeledShapeWithShadow shape label name point =
>       --draw the label
>       (placeObject (text label # fontSizeL 1.0 # fc black) point)
>       --draw the box
>    <> (placeObject (shape # fc white # lw thin # maybeNamed name) point)
>       --draw the shadow
>    <> (placeObject (shape # fc grey # lw none) (offset point))
>   where
>       offset (x, y) = (x+0.25, y-0.25)

The `labeledShapeWithShadow'` method extracts the shape transformation logic
from the original `labeledShapeWithShadow` so that the shape attributes can be
adjusted.

> labeledShapeWithShadow' :: Diagram B -> (Diagram B -> Diagram B) -> String
>                            -> (Double, Double) -> Diagram B
> labeledShapeWithShadow' shape shapeTransform label point =
>       --draw the label
>       (placeObject (text label # fontSizeL 1.0 # fc black) point)
>       --draw the box
>    <> (placeObject (shapeTransform shape) point)
>       --draw the shadow
>    <> (placeObject (shape # fc grey # lw none) (offset point))
>   where
>       offset (x, y) = (x+0.25, y-0.25)

The `roundedBox` method uses the above shape drawing method to draw a rounded
rectangle.

> roundedBox :: String -> Maybe String -> (Double, Double) -> Diagram B
> roundedBox label name point =
>       labeledShapeWithShadow (roundedRect 16 5 0.5) label name point

The `diamondBox` method draws a shaded diamond box.

> diamondBox :: String -> Maybe String -> (Double, Double) -> Diagram B
> diamondBox label name point =
>       labeledShapeWithShadow' diamondBox
>                               ((fc lightgrey).(lw thin).(maybeNamed name))
>                               label point
>   where
>       diamondBox = center $ strokeLoop $ closeLine $ fromOffsets $ map r2 $
>                       [(-8.0,  0.0), (-4.0,  2.5), ( 4.0,  2.5),
>                        ( 8.0,  0.0), ( 4.0, -2.5), (-4.0, -2.5)]

The `squareBox` method is identical to the `roundedBox` method, but with sharp
corners.

> squareBox :: String -> Maybe String -> (Double, Double) -> Diagram B
> squareBox label name point =
>       labeledShapeWithShadow (rect 16 5) label name point

The `floatingLabel` method draws a label in free space at the given coordinates.

> floatingLabel :: String -> (Double, Double) -> Diagram B
> floatingLabel label point =
>       (placeObject (text label # fontSizeL 2.0
>                                # fc black)
>                    point)

The `dashedFence` method creates a dashed line with the given ratio of on and
off pixels.  The dashed line connects the given list of vertices, and is placed
at the given start point in the diagram.

> dashedFence :: Double -> Double -> [(Double, Double)] -> (Double, Double)
>               -> Diagram B
> dashedFence on off points location =
>       placeObject ((strokeLine $ lineFromVertices $ map p2 points)
>                       # lc grey
>                       # dashingG [on*scale, off*scale] 0)
>                   location
>   where
>       scale = 0.25

The `topFence` method creates a thick dark line between the given list of
vertices, and is placed at the given start point in the diagram.

> topFence :: [(Double, Double)] -> (Double, Double) -> Diagram B
> topFence points location =
>       placeObject ((strokeLine $ lineFromVertices $ map p2 points)
>                       # lc black
>                       # lw thick)
>                   location
