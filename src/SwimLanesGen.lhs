SwimLanes Code Generator
========================

This module converts a `SwimLanes` document into a Diagrams `Diagram`, which is
suitable for displaying using one of the Diagrams backends.  The process for
converting the two is rather involved, and first requires performing some
semantic analysis on the `SwimLanes` document to both normalize the document and
detect any errors.  Then some measurements of the swim lanes grid are taken in
order to compute the points on the grid needed for drawing.  Functions for
styling elements within the diagram are generated using the various `Style`
structures in the document.  Finally, the complete diagram is drawn by
traversing the analyzed document and placing each element where it belongs.

> {-# LANGUAGE NoMonomorphismRestriction #-}
>
> module SwimLanesGen (
>   swimLaneAnalyze, drawSwimLanes
> ) where

We need `Control.Monad` for working with monads.

> import Control.Monad

We need `Data.Maybe` for various functions working with the `Maybe` data type.

> import Data.Maybe

We need `Data.Map.Lazy` to work with maps.

> import qualified Data.Map.Lazy as Map

We need `Parsec` for dealing with parsing tasks.

> import Text.Parsec.Error
> import Text.Parsec.Pos

We use the Diagrams package for drawing pictures.

> import Diagrams.Prelude hiding (Path, Wrapped, pathVertices)
> import Diagrams.Backend.Postscript.CmdLine

We will be working with the SwimLanes document, so we import it.

> import SwimLanes

We use the `Shapes` module for drawing shapes.

> import Shapes

Semantic Analysis
=================

Our first task is to normalize and then analyze the document structure.  This
will make it easier for us to draw the document later on.  This process requires
us to resolve labels and style references across the document.

We use the type, `Diagram B`, as our `Graphics` type.

> type Graphics = Diagram B

Error Helpers
-------------

Before delving into semantic analysis, we create some helper methods for
creating error messages.

An `unknownStyleError` is returned when a referenced style does not exist.

> unknownStyleError :: SourcePos -> String -> ParseError
> unknownStyleError pos style =
>       newErrorMessage (Message ("Undefined style " ++ style)) pos

An `unknownCellLabelError` is returned when a referenced cell does not exist.

> unknownCellLabelError :: SourcePos -> String -> ParseError
> unknownCellLabelError pos label =
>       newErrorMessage (Message ("Undefined label " ++ label)) pos

A `duplicateLabelError` is returned when two cells with the same label are
encountered when traversing the document.

> duplicateLabelError :: Cell Graphics -> ParseError
> duplicateLabelError cell =
>       newErrorMessage (Message ("Duplicate label " ++ label)) pos
>   where
>       label = wrappedValue   .fromJust.cellLabel $ cell
>       pos   = wrappedPosition.fromJust.cellLabel $ cell

Path Label Resolution
---------------------

First, we iterate through each `Lane` and add all labeled `Cell`s to the
`swimLaneLabels` structure.  To do this, we create a helper function called
`extractCells`, which extracts all cells from the document.
 
> extractCells :: SwimLanes Graphics -> [Wrapped (Cell Graphics)]
> extractCells swim =
>   concatMap (\l -> laneCells $ wrappedValue l) (swimLanes swim)

A second helper function, `cellHasLabel`, returns `True` if a cell has a label.
We unwrap the cell, extract the label field, and see if it is set.

> cellHasLabel :: Wrapped (Cell Graphics) -> Bool
> cellHasLabel = isJust.cellLabel.wrappedValue

Now we can fold the labels into our SwimLanes document using the `Either`
monad.  This allows us to display errors back to the user if we encounter a
duplicate label.

> extractLabels :: SwimLanes Graphics -> Either ParseError (SwimLanes Graphics)
> extractLabels swim =
>       foldM uniqueInsert swim $ filter cellHasLabel $ extractCells swim
>   where
>       --either insert a new value, or raise a duplicate label error
>       uniqueInsert swim wc@(Wrapped pos cell) =
>           case Map.lookup (wrappedValue.fromJust.cellLabel $ cell)
>                           (swimLaneLabels swim) of
>               Nothing  -> Right $ insertCell swim wc
>               Just _   -> Left  $ duplicateLabelError cell
>       --insert a cell label into the swim lanes document
>       insertCell swim wc@(Wrapped pos cell) =
>           swim {
>               swimLaneLabels =
>                   Map.insert (wrappedValue.fromJust.cellLabel $ cell) wc
>                              (swimLaneLabels swim) }

Now, we need to normalize our `Path` structures to expand references to any
`Cell`s with the actual `Cell`s.  To do this, we write a method that transforms
a given `PathVertex` to a `PVCellLabel` using the provided `SwimLanes` document.

> pathVertexTransform :: SwimLanes Graphics -> PathVertex Graphics
>                        -> Either ParseError (PathVertex Graphics)
> pathVertexTransform    _ c@(PVCell _)                        = Right c
> pathVertexTransform swim   (PVCellLabel (Wrapped pos label)) =
>       case Map.lookup label $ swimLaneLabels swim of
>           Just wcell   -> Right $ PVCell wcell
>           Nothing      -> Left  $ unknownCellLabelError pos label

We can now build a `pathLabelTransform` that performs the above transformation
on a `Wrapped Path`.

> pathLabelTransform :: SwimLanes Graphics -> Wrapped (Path Graphics)
>                       -> Either ParseError (Wrapped (Path Graphics))
> pathLabelTransform swim (Wrapped pos path) = do
>       vertices <- mapM (pathVertexTransform swim) (pathVertices path)
>       return (Wrapped pos $ path { pathVertices = vertices })

We can now generalize this transformation to entire swim lane documents.

> swimLanePathLabelTransform :: SwimLanes Graphics
>                               -> Either ParseError (SwimLanes Graphics)
> swimLanePathLabelTransform swim = do
>       paths <- mapM (pathLabelTransform swim) (swimLanePaths swim)
>       return swim { swimLanePaths = paths }

Cell Style Reference Resolution
-------------------------------

For our second pass of analysis, we resolve style references in all cells.
Style references are optional.  Later on, if we cannot find a style override for
a cell, we will use the default style for that cell.

As before, we will perform this analysis from the bottom up.  First, we create a
function that resolves a cell style from a reference.  This transformation
function combines three possibilities.  If a cell style is not referenced, then
it returns the original cell.  If a cell style is referenced, then this
reference is looked up in the map.  If the style exists, then the cell is
updated with a reference to that style.  If the style does not exist, then an
error is bubbled up to the user.

> cellStyleTransform :: (SwimLanes Graphics) -> Wrapped (Cell Graphics)
>                       -> Either ParseError (Wrapped (Cell Graphics))
> cellStyleTransform swim wc@(Wrapped pos cell) =
>       case lookupStyleRef cell of
>              Left err         -> Left err
>              Right (Just ws)  -> Right $
>                                       Wrapped pos (cell {cellStyle = Just ws})
>              Right Nothing    -> Right wc
>   where
>       lookupStyleRef (Cell _ _ _ _ (Just (Wrapped pos style)) _) =
>           case Map.lookup style (swimLaneStyles swim) of
>               res@(Just ws) -> Right res
>               Nothing       -> Left $ unknownStyleError pos style
>       lookupStyleRef _ = Right Nothing

We can now generalize this transformation to work on a `Lane` by mapping this
transform.

> laneCellStyleTransform :: SwimLanes Graphics -> Wrapped (Lane Graphics)
>                           -> Either ParseError (Wrapped (Lane Graphics))
> laneCellStyleTransform swim (Wrapped pos lane) = do
>       cells <- mapM (cellStyleTransform swim) (laneCells lane)
>       return (Wrapped pos $ lane { laneCells = cells })

Finally, we generalize this transformation to work on the entire swim lanes
document.

> swimLaneCellStyleTransform :: (SwimLanes Graphics)
>                               -> Either ParseError (SwimLanes Graphics)
> swimLaneCellStyleTransform swim = do
>       lanes <- mapM (laneCellStyleTransform swim) (swimLanes swim)
>       return swim { swimLanes = lanes }

Lane Style Reference Resolution
-------------------------------

For our third pass of analysis, we resolve style references in all lanes.
Style references are optional.  Later on, if we cannot find a style override for
a lane, we will use the default style for that lane.

The first function resolves a lane style from a reference.  This transformation
function combines three possibilities, like the cell style transformation
function.  If a lane style is not referenced, then it returns the original lane.
If a lane style is referenced, then this reference is looked up in the map.  If
the style exists, then the lane is updated with a reference to that style.  If
the style does not exist, then an error is bubbled up to the user.

> laneStyleTransform :: (SwimLanes Graphics) -> Wrapped (Lane Graphics)
>                       -> Either ParseError (Wrapped (Lane Graphics))
> laneStyleTransform swim wl@(Wrapped pos lane) =
>       case lookupStyleRef lane of
>               Left err        -> Left err
>               Right (Just ws) -> Right $
>                                       Wrapped pos (lane {laneStyle = Just ws})
>               Right Nothing   -> Right wl
>   where
>       lookupStyleRef (Lane _ _ _ (Just (Wrapped pos style)) _) =
>           case Map.lookup style (swimLaneStyles swim) of
>               res@(Just ws) -> Right res
>               Nothing       -> Left $ unknownStyleError pos style
>       lookupStyleRef _ = Right Nothing

We can now build a high-level transformation that works on the entire swim lanes
document.

> swimLaneLaneStyleTransform :: (SwimLanes Graphics)
>                               -> Either ParseError (SwimLanes Graphics)
> swimLaneLaneStyleTransform swim = do
>       lanes <- mapM (laneStyleTransform swim) (swimLanes swim)
>       return swim { swimLanes = lanes }

Path Style Reference Resolution
-------------------------------

For our fourth pass of analysis, we resolve style references in all paths.
Style references are optional.  Later on, if we cannot find a style override for
a path, we will use the default style for that path.

The first function resolves a path style from a reference.  This transformation
function combines three possibilities, like both the cell style and lane style
transformation functions.  If a path style is not referenced, then it returns
the original path.  If a path style is referenced, then this reference is looked
up in the map.  If the style exists, then the path is updated with a refreence
to that style.  If the style does not exist, then an error is bubbled up to the
user.

> pathStyleTransform :: SwimLanes Graphics -> Wrapped (Path Graphics)
>                       -> Either ParseError (Wrapped (Path Graphics))
> pathStyleTransform swim wp@(Wrapped pos path) =
>       case lookupStyleRef path of
>               Left err        -> Left err
>               Right (Just ws) -> Right $
>                                       Wrapped pos (path {pathStyle = Just ws})
>               Right Nothing   -> Right wp
>   where
>       lookupStyleRef (Path _ (Just (Wrapped pos style)) _) =
>           case Map.lookup style (swimLaneStyles swim) of
>               res@(Just ws) -> Right res
>               Nothing       -> Left $ unknownStyleError pos style
>       lookupStyleRef _ = Right Nothing

We can now build a high-level transformation that works on the entire swim lanes
document.

> swimLanePathStyleTransform :: (SwimLanes Graphics)
>                               -> Either ParseError (SwimLanes Graphics)
> swimLanePathStyleTransform swim = do
>       paths <- mapM (pathStyleTransform swim) (swimLanePaths swim)
>       return swim { swimLanePaths = paths }


Complete Semantic Analysis Function
-----------------------------------

We can now combine all semantic analysis steps into a single pipeline.  This
pipeline executes each transformation in sequence.  We make use of the `Either`
monad to stitch these pipeline steps together so that errors can be bubbled up
to the user.

> swimLaneAnalyze :: (SwimLanes Graphics)
>                    -> Either ParseError (SwimLanes Graphics)
> swimLaneAnalyze swim =
>           swimLanePathStyleTransform 
>       =<< swimLaneLaneStyleTransform
>       =<< swimLaneCellStyleTransform
>       =<< swimLanePathLabelTransform
>       =<< extractLabels swim

At this point, we have a normalized document ready to draw, or an error message
that we can provide to the user regarding a potential issue with the document.

Drawing the Swim Lanes Document
===============================

**During this initial version of the Swim Lanes document, sizes of elements are
fixed.  In later versions of this document, this will change, and these sizes
will become the defaults.**

Drawing Cells
-------------

The first step of drawing the document is to draw all of the cells.  We need to extract the lanes and cells into a list of lists.

> extractCellGrid :: SwimLanes Graphics -> [[Wrapped (Cell Graphics)]]
> extractCellGrid swim =
>       map (laneCells.wrappedValue) $ swimLanes swim

Each element in the grid is 16x5 units in size.  The center points for each
element is 24 units on the X and 10 units on the Y apart.  Since we want our
diagram to grow down, the Y units are negative.  This function takes our list of
lists and modifies it into a list of tuples that have the right X coordinate.
Within each tuple is the Y coordinate for an individual cell along with the cell
itself.

> placeCellGrid :: [[Wrapped (Cell Graphics)]]
>                   -> [(Double, [(Double, Cell Graphics)])]
> placeCellGrid grid =
>       zip (iterate (+xOffset) 0) $
>           map (\c -> zip (iterate (+yOffset) 0) $ map wrappedValue c) grid
>   where
>       xOffset = 24.0
>       yOffset = (-10.0)

The `collapseCellGrid` method collapses the above list of tuples, giving a list
of 3-tuples that contain the X, Y, and Cells for each member of the grid.

> collapseCellGrid :: [(Double, [(Double, Cell Graphics)])]
>                     -> [(Double, Double, Cell Graphics)]
> collapseCellGrid grid =
>       concat $ map (\(x, lst) -> map (\(y, cell) -> (x, y, cell)) lst) grid

We need some helper functions for drawing the cell.  The first helper function,
`cellToLabel`, takes a cell and returns the label to use when drawing the cell.

> cellToLabel :: Cell Graphics -> String
> cellToLabel cell =
>       case cellType cell of
>           CellTypeEmpty       -> ""
>           CellTypeBox         -> fromMaybe "" $
>                                       (wrappedValue <$> cellText cell)
>           CellTypeDiamond     -> fromMaybe "" $
>                                       (wrappedValue <$> cellText cell)
>           CellTypeSquareBox   -> fromMaybe "" $
>                                       (wrappedValue <$> cellText cell)
>           CellTypeEllipsis    -> "..."

The `drawCell` method transforms a cell into a Diagram ready to be placed.  This
method requires the X and Y coordinates and the cell to draw.

> drawCell :: Double -> Double -> Cell Graphics -> Graphics
> drawCell x y cell =
>       drawBox (cellToLabel cell) (wrappedValue <$> (cellLabel cell)) (x, y)
>   where
>       drawBox = case cellType cell of
>                   CellTypeEmpty       -> (\_ _ _ -> mempty)
>                   CellTypeBox         -> roundedBox
>                   CellTypeDiamond     -> diamondBox
>                   CellTypeSquareBox   -> squareBox
>                   CellTypeEllipsis    -> roundedBox

The `drawCells` method draws all cells in the collapsed cell grid.

> drawCells :: [(Double, Double, Cell Graphics)] -> Graphics
> drawCells cgrid =
>       foldl (<>) mempty $ map (\(x, y, cell) -> drawCell x y cell) cgrid

The `drawSwimLaneCells` method combines the extract and collapse work above with
the `drawCells` method.

> drawSwimLaneCells :: SwimLanes Graphics -> Graphics
> drawSwimLaneCells swim =
>       drawCells $ collapseCellGrid $ placeCellGrid $ extractCellGrid swim

Drawing Paths
-------------

Paths are significantly easier than cells, because much of the work in drawing
paths is handled by `Diagrams` thanks to its rather powerful arrow framework.
The first function we need is a `drawPath` function, which recursively draws a
path using the list of path vertices.  The result of this method is a function
that takes a diagram and returns a diagram with all of the path arrows drawn.

> drawPath :: Path Graphics -> (Graphics -> Graphics)
> drawPath path =
>       foldl (.) id $ drawArrows [] (pathVertices path)
>   where
>       drawArrows lst ((PVCell (Wrapped _ a)) :
>                           as@((PVCell (Wrapped _ b)) : bs)) =
>           drawArrows ((connectOutside' (with & headLength .~ verySmall)
>                                        ((wrappedValue.fromJust.cellLabel) a)
>                                        ((wrappedValue.fromJust.cellLabel) b))
>                        : lst)
>                      as
>       drawArrows lst _ = lst

We then use this method to draw all paths in a swim lanes document.

> drawPaths :: SwimLanes Graphics -> (Graphics -> Graphics)
> drawPaths swim =
>       foldl (.) id $ map (drawPath.wrappedValue) (swimLanePaths swim)

Drawing Lane Fences
-------------------

Between each lane, we want to draw a fence.  A fence is a dashed line that
separates lanes to show the separation between parallel processes.  To draw
fences, we first create an `extractLanes` method which extracts lanes and
creates a list of tuples with the center point of the lane and the lane itself.

> extractLanes :: SwimLanes Graphics -> [(Double, Lane Graphics)]
> extractLanes swim =
>       zip (iterate (+xOffset) 0) $ map wrappedValue $ swimLanes swim
>   where
>       xOffset = 24.0

We need to know the maximum height of each lane.  The `deepestCellY` method
performs a linear mapping based on the number of cells in each lane.  The
deepest of these (the minimum Y coordinate) is returned.

> deepestCellY :: SwimLanes Graphics -> Double
> deepestCellY swim =
>       (+) (-5.0) $ minimum $ map (\l -> (l - 1.0) * yOffset) $
>           map (fromIntegral.length) $
>           map (laneCells.wrappedValue) $ swimLanes swim
>   where
>       yOffset = (-10.0)

To draw each fence, we recurse through the the list of lane tuples, drawing a
fence between each lane.

> drawLaneFences :: SwimLanes Graphics -> Graphics
> drawLaneFences swim =
>       foldl (<>) mempty $ drawLaneFence [] $ map fst $ extractLanes swim
>   where
>       drawLaneFence lst (a : as@(b : bs)) =
>           drawLaneFence
>               ((dashedFence 2.0 1.0
>                             [(midpoint a b, 5.0), (midpoint a b, lowPoint)]
>                             (midpoint a b, 5.0))
>               : lst)
>               as
>       drawLaneFence lst _ = lst
>       midpoint x1 x2 = x1 + ((x2 - x1) / 2)
>       lowPoint = deepestCellY swim

Drawing the Top Header
----------------------

The top header for a swim lanes document is made of a vertical bar separating
the header from the rest of the document, and the labels for each lane.

By re-using the `extractLanes` method in the previous section, we can extract
each lane and its X midpoint to aid with placing the lane labels.  Each lane
label starts 15 units above the first cell in a lane.  The `drawLaneLabel`
method takes a tuple of the X midpoint and the lane, and draws the lane label.

> drawLaneLabel :: (Double, Lane Graphics) -> Graphics
> drawLaneLabel (xmid, lane) =
>       floatingLabel ((wrappedValue.laneName) lane) (xmid, 7.5)

The `drawLaneLabels` method draws labels for each lane in a swim lanes document.

> drawLaneLabels :: SwimLanes Graphics -> Graphics
> drawLaneLabels swim =
>       foldl (<>) mempty $ map drawLaneLabel $ extractLanes swim

The `drawTopHorizontalRule` method draws a horizontal rule for the swim lanes
header.  We take the first and last elements in the lanes for a swim lanes
document, and use these to compute the offsets for the horizontal rule.

> drawTopHorizontalRule :: SwimLanes Graphics -> Graphics
> drawTopHorizontalRule swim =
>       topFence [beginCoord, endCoord] beginCoord
>   where
>       beginCoord = (firstLaneX - 12.0, 5.0)
>       endCoord   = (lastLaneX  + 12.0, 5.0)
>       firstLaneX = fst $ head $ extractLanes swim
>       lastLaneX  = fst $ last $ extractLanes swim

Finally, we combine each drawing method into a `drawTopHeader` method.

> drawTopHeader :: SwimLanes Graphics -> Graphics
> drawTopHeader swim =
>       drawTopHorizontalRule swim <> drawLaneLabels swim

Drawing the Swim Lanes Document
-------------------------------

We can combine all drawing passes using the function, `drawSwimLanes`, which
takes a swim lanes document and returns a diagram.

> drawSwimLanes :: SwimLanes Graphics -> Graphics
> drawSwimLanes swim =
>       (drawPaths swim) $ frame 1.5 $
>               drawTopHeader swim
>            <> drawSwimLaneCells swim
>            <> drawLaneFences swim
