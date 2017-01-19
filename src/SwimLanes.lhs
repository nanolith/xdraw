Swim Lanes
==========

This module describes the swim lanes data format which is parsed from
`.swimlane` files.

> module SwimLanes (
>       SwimLanes (SwimLanes, swimLaneLabels, swimLanes, swimLanePaths,
>                  swimLaneStyles),
>       Cell (Cell, cellGraphics, cellLabel, cellStyle, cellStyleRef, cellType,
>             cellText),
>       CellType (CellTypeEmpty, CellTypeBox, CellTypeDiamond,
>                 CellTypeSquareBox, CellTypeEllipsis),
>       Lane (Lane, laneGraphics, laneName, laneStyle, laneStyleRef, laneCells),
>       emptyLane,
>       Path (Path, pathStyle, pathStyleRef, pathVertices), emptyPath,
>       PathVertex (PVCellLabel, PVCell),
>       Style (Style, styleName, styleElements),
>       StyleElement (SEColor, SELine, SEBeginSegment, SEEndSegment),
>       ColorType (ColorTypeName, ColorTypeRGB),
>       LineStroke (LSNamed, LSDashed, LSSolid),
>       SegmentType (STStraight, STNamed),
>
>       parseSegmentType, parseLineStroke, parseColorType, parseStyleElement,
>       parseStyle, parsePath, parseLane, parseSwimLanes
> ) where

We import `Data.Map` which provides the `Map` type used to track labels.

> import qualified Data.Map.Lazy as Map

We use `Data.List` for list operations.

> import Data.List

We use `Data.Maybe` for some `Map` operations.

> import Data.Maybe

We use Parsec to build up a parser from parser combinators.

> import Text.Parsec hiding (State)
> import Text.Parsec.String (Parser)

We use the position type from Parsec Pos.

> import qualified Text.Parsec.Pos as Pos

We use our internal Lexer to tokenize input.

> import Lexer

We use the Applicative Functor typeclass to describe some of our parser
combinators.

> import Control.Applicative hiding ((<|>), many, optional)

We use Either to handle blocks that are interspersed.

> import Data.Either

We use our `Wrapped` module to wrap data with source position information.

> import Wrapped

SwimLanes Data Type
-------------------

The `SwimLanes` data type provides an AST view into a swim lane document.  A
swim lane document contains a dictionary of labels to individual cells, a list
of swim lanes, a list of paths, and a dictionary styles.  This is a
parameterized type, which allows us to isolate this parser from the graphics
back-end.

> data SwimLanes g = SwimLanes {
>                       swimLaneLabels :: Map.Map String (Wrapped (Cell g)),
>                       swimLanes :: [Wrapped (Lane g)],
>                       swimLanePaths :: [Wrapped (Path g)],
>                       swimLaneStyles :: Map.Map String (Wrapped Style)
>                   } deriving (Eq, Show)

The `Cell` data type represents a single cell in the swim lanes diagram.  This
cell has an optional graphics component, an optional style and style label, and
a type.  The graphics and style are populated during later analysis.  The cell
style label can be provided by the user to override the default style of a cell.
The cell type builds the drawable material of the cell.  The `cellGraphics`
accessor represents the graphical portion of this cell, which is populated
during drawing passes.

> data Cell g = Cell {
>               cellGraphics :: Maybe g,
>               cellLabel :: Maybe (Wrapped String),
>               cellText :: Maybe (Wrapped String),
>               cellStyle :: Maybe (Wrapped Style),
>               cellStyleRef :: Maybe (Wrapped String),
>               cellType :: CellType
>             } deriving (Eq, Show)

The `CellType` data structure contains the drawable information for this cell.
A cell can be empty, a box cell with a string representing the contents, or an
ellipsis.

> data CellType = CellTypeEmpty
>               | CellTypeBox
>               | CellTypeDiamond
>               | CellTypeSquareBox
>               | CellTypeEllipsis
>               deriving (Eq, Show)

The `Lane` data type represents a lane of cells in the swim lane diagram.  This
data type contains a list of wrapped cells, an optional graphical component, and
an optional style and / or style reference.

> data Lane g = Lane {
>               laneGraphics :: Maybe g,
>               laneName :: Wrapped String,
>               laneStyle :: Maybe (Wrapped Style),
>               laneStyleRef :: Maybe (Wrapped String),
>               laneCells :: [Wrapped (Cell g)]
>             } deriving (Eq, Show)

We create a default constructor for `Lane` which creates an empty lane that can
be modified as it is parsed.

> emptyLane :: Wrapped String -> Lane g
> emptyLane name = Lane Nothing name Nothing Nothing []

A `Path` is a list of path vertices.  There is a `pathStyle` and `pathStyleRef`
which represent the optional style for a path (populated during semantic
analysis) and a symbolic reference to this style.

> data Path g = Path {
>               pathStyle :: Maybe (Wrapped Style),
>               pathStyleRef :: Maybe (Wrapped String),
>               pathVertices :: [PathVertex g]
>             } deriving (Eq, Show)

Parsing a `Path` is a bit different than parsing other elements, because each
statement within a path is a modifier of the path.  As such, we must start with
an `emptyPath` and then fold each path element into the final path.  This
constructor returns an empty path.

> emptyPath :: Path g
> emptyPath =
>       Path Nothing Nothing []

A `PathVertex` is either a `PVCellLabel` or a `PVCell`.  A `PVCellLabel` points
to a cell by label.  A `PVCell` contains the cell itself.  The latter type is
populated during analysis from `PVCellLabel` values.

> data PathVertex g = PVCellLabel (Wrapped String) | PVCell (Wrapped (Cell g))
>                     deriving (Eq, Show)

A `Style` has a name and a list of `StyleElement` values.

> data Style = Style {
>                   styleName :: Wrapped String,
>                   styleElements :: [Wrapped StyleElement]
>               } deriving (Eq, Show)

A `StyleElement` describes one aspect of a style.  This includes properties such
as color, line stroke, and begin and end segment descriptors.

> data StyleElement = SEColor (Wrapped ColorType)
>                   | SELine (Wrapped LineStroke)
>                   | SEBeginSegment (Wrapped SegmentType)
>                   | SEEndSegment (Wrapped SegmentType)
>                   deriving (Eq, Show)

The `ColorType` data type describes a color by name or RGB value.  The
`ColorTypeName` constructor takes a symbolic name for a color.  The
`ColorTypeRGB` constructor takes three double values between 0.0 and 1.0
representing the red, blue, and green components of the color.

> data ColorType = ColorTypeName String | ColorTypeRGB Double Double Double
>                  deriving (Eq, Show)

The `LineStroke` type describes the stroke of the line.  Lines can be dashed or
solid, with a width.  The `LSDashed` constructor contains the weight, on length,
and off length.  The `LSSolid` constructor contains the weight only.  The
`LSNamed` constructor provides a way for the particulars of the line segment to
be named symbolically.  It includes one integer value, generally for the weight.

> data LineStroke = LSNamed String Int | LSDashed Int Int Int | LSSolid Int
>                   deriving (Eq, Show)

`SegmentType` is used to set the begin and end segment types.  The segment type
can either be straight or a named type.  The named type has a primary name (e.g.
"arrow") and an optional subtype (e.g. "small").

> data SegmentType = STStraight | STNamed String (Maybe String)
>                    deriving (Eq, Show)

Parsing SwimLanes Documents
===========================

We take a bottom up approach for parsing swim lanes documents.  First we start
with the leaf nodes of the AST, and combine these into more and more complete
pieces of the overall document.  We will build the top-level statement parsers
from the bottom up.  First, we will focus on `Style`, then `Path`, then
`Lane`, and finally we will combine all of these top level definitions into our
`parseSwimLanes` parser.

Parsing Style Definitions
-------------------------

In this section, we build the `parseStyle` parser from the bottom up.

The `parseSegmentType` method parses a segment type, returning it as a `Wrapped
SegmentType`.  This parse method attempts three different parse paths.  First,
it attempts to parse a `straight` value, representing an unadorned line segment.
If this fails, it attempts to parse a named value with a subtype separated by a
dot.  If this fails, then it attempts to parse a named value without a subtype.

> parseSegmentType :: Parser (Wrapped SegmentType)
> parseSegmentType =
>       Wrapped <$> getPosition
>               <*> (     try (reserved "straight" >> pure STStraight)
>                     <|> try (STNamed <$> identifier
>                                      <*> (Just <$> (dot >> identifier)))
>                     <|>     (STNamed <$> identifier <*> pure Nothing))

The `parseLineStroke` method parses line strokes.  There are three line strokes
supported: `solid`, `dashed`, and a named type.  The `solid` stroke accepts one
bracketed int parameter indicating the weight of the line.  The `dashed` stroke
accepts three bracketed int parameters representing the weight, on stride, and
off stride for a dashed line.  We first attempt to parse a `solid` stroke,
falling back to a `dashed` stroke if that fails.  The `dashed` subparser
attempts to parse three ints in brackets.  These are stuffed into a 3-tuple,
which is then decomposed by pattern matching.  If both of these parses fail,
then we attempt to parse a named stroke with a single bracketed int parameter.

> parseLineStroke :: Parser (Wrapped LineStroke)
> parseLineStroke =
>     Wrapped <$> getPosition
>             <*> (     try (reserved "solid" >> (LSSolid <$> brackets int))
>                   <|> try (reserved "dashed" >>
>                             (\(a,b,c) -> LSDashed a b c) <$> brackets (
>                                    (,,) <$> int
>                                         <*> (comma >> int)
>                                         <*> (comma >> int)))
>                   <|> (LSNamed <$> identifier
>                                <*> brackets int))

The `parseColorType` parser creates a `Wrapped ColorType` from parsed input.
Two types of colors are supported, `color name` which allows us to specify a
named color, and `colorRGB [r,g,b]` which allows an RGB color to be specified
using floating point values between 0 and 1.  We first attempt to parse a named
color, and if this fails, then we attempt to parse an RGB color.

> parseColorType :: Parser (Wrapped ColorType)
> parseColorType =
>     Wrapped <$> getPosition
>             <*> (     try (reserved "color" >>
>                             (ColorTypeName <$> identifier))
>                   <|> try (reserved "colorRGB" >>
>                             ((\(r,g,b) -> ColorTypeRGB r g b) <$> brackets
>                                ((,,) <$> float
>                                      <*> (comma >> float)
>                                      <*> (comma >> float)))))

We combine `parseSegmentType`, `parseLineStroke`, and `parseColorType` into a
`parseStyleElement` parser which parses any single style element.  First we try
to parse a color type, then a line stroke, and finally a begin or end segment
type.

> parseStyleElement :: Parser (Wrapped StyleElement)
> parseStyleElement =
>   Wrapped <$> getPosition
>           <*> (     try (SEColor <$> parseColorType)
>                 <|> try (SELine <$> (reserved "line" >> parseLineStroke))
>                 <|> try (SEBeginSegment <$> (reserved "begin" >>
>                                                           parseSegmentType))
>                 <|>     (SEEndSegment <$> (reserved "end" >>
>                                                           parseSegmentType)))

We can use the `parseStyleElement` method to build our `parseStyle` method,
which represents our first top-level definition.  A style has a name and zero or
more style elements which define the appearance of the style.

> parseStyle :: Parser (Wrapped Style)
> parseStyle =
>   Wrapped <$> getPosition
>           <*> (Style <$> (    reserved "style"
>                            >> (Wrapped <$> getPosition <*> identifier))
>                      <*> braces (many parseStyleElement))

Parsing Path Definitions
------------------------

We take a different approach to parsing path and lane definitions.  Because
statements within these definitions can modify the data structure (e.g. changing
the style, width, or other elements of the structure), we fold these statements
into the final data structure using a left fold.  We start by building the
`pathStyleParser`.

The `pathStyleParser` accepts a style reference and returns a function that maps
a `Path` to a `Path` with the given style.

> pathStyleParser :: Parser ((Path g) -> (Path g))
> pathStyleParser =
>       reserved "style" >> (pathStyleXform.Just <$> (Wrapped <$> getPosition
>                                                             <*> identifier))
>   where
>       pathStyleXform id path = path { pathStyleRef = id }

The `pathVertexParser` parses a single vertex for a `Path`.

> pathVertexParser :: Parser (PathVertex g)
> pathVertexParser =
>       PVCellLabel <$> (Wrapped <$> getPosition <*> identifier)

The `pathEdgesParser` parses a set of vertices into edges.  It returns a
function that maps a `Path` into a `Path` with the given edges.

> pathEdgesParser :: Parser ((Path g) -> (Path g))
> pathEdgesParser =
>       pathEdgesXform <$> (pathVertexParser `sepBy1` reservedOp "-->")
>   where
>       pathEdgesXform vertices path =
>               path {
>                   pathVertices = (pathVertices path) ++ vertices }

We can combine the path mapping parsers to form a parser that will accept all
statements in a path top level definition.  We call this method
`parsePathStatements`.

> parsePathStatements :: Parser [(Path g) -> (Path g)]
> parsePathStatements =
>       many (try pathStyleParser <|> pathEdgesParser)

We can use the `parsePathStatements` parser to parse the entire path top-level
definition.  We must take the list of statements and fold them into the correct
type of path to create a Path definition.  This is done using a left fold.  The
complete parser for this definition is the `parsePath` function below.

> parsePath :: Parser (Wrapped (Path g))
> parsePath =
>   Wrapped <$> getPosition
>           <*> (reserved "path" >> foldedPathDefs)
>   where
>       foldedPathDefs = braces $ folded
>       folded =
>           foldl <$> pure foldPathDefinitions
>                 <*> pure emptyPath
>                 <*> parsePathStatements
>       foldPathDefinitions path stmt = stmt path

Parsing Lane Definitions
------------------------

Like `Path`, a `Lane` uses a folded parsing strategy to fold statements within a
lane into the `Lane` structure.

The `laneStyleParser` accepts a style reference and returns a function that maps
a `Lane` to a `Lane` with the given style.

> laneStyleParser :: Parser ((Lane g) -> (Lane g))
> laneStyleParser =
>       reserved "style" >> (laneStyleXform.Just <$> (Wrapped <$> getPosition
>                                                             <*> identifier))
>   where
>       laneStyleXform id lane = lane { laneStyleRef = id }

The `cellLabelParser` parses an optional cell label, returning either a function
that maps a `Cell` to a `Cell` with the given label, or the `id` function for
`Cell` structures.

> cellLabelParser :: Parser ((Cell g) -> (Cell g))
> cellLabelParser =
>       option id (try labelTransformer)
>   where
>       labelTransformer = transformLabel <$> (Wrapped <$> getPosition
>                                                      <*> identifier <* colon)
>       transformLabel label cell = cell { cellLabel = Just label }

The `addCellToLane` helper function adds a cell to a lane, first applying the
given cell mapping function for convenience.

> addCellToLane :: Wrapped (Cell g) -> ((Cell g) -> (Cell g)) -> Lane g
>                  -> Lane g
> addCellToLane cell cellXform lane =
>       lane { laneCells = (laneCells lane) ++ [wrappedXform cellXform cell] }
>   where
>       wrappedXform xform (Wrapped pos cell) = Wrapped pos $ xform cell

The `ellipsisCellParser` parses an ellipsis cell, generating a function mapping
a `Lane` to a `Lane` with the given cell.

> ellipsisCellParser :: Parser ((Lane g) -> (Lane g))
> ellipsisCellParser =
>       laneCellXformer <$> cellLabelParser
>                       <*> (getPosition <* (reservedOp "..."))
>   where
>       laneCellXformer cellLabeler pos =
>           addCellToLane
>             (Wrapped pos $
>                 Cell Nothing Nothing Nothing Nothing Nothing CellTypeEllipsis)
>             cellLabeler

The `emptyCellParser` parses an empty cell, generating a function mapping a
`Lane` to a `Lane` containing the cell.

> emptyCellParser :: Parser ((Lane g) -> (Lane g))
> emptyCellParser =
>       laneCellXformer <$> cellLabelParser
>                       <*> (getPosition <* (reserved "empty"))
>   where
>       laneCellXformer cellLabeler pos =
>         addCellToLane
>             (Wrapped pos $
>                 Cell Nothing Nothing Nothing Nothing Nothing CellTypeEmpty)
>             cellLabeler

In order to parse boxes, we must build a set of parsers that modify a `Cell`
structure to add properties based on parsed input.  The first of these parsers
is the `cellTextParser` which parses a text block.

> cellTextParser :: Parser ((Cell g) -> (Cell g))
> cellTextParser =
>       cellXform <$> (    (reserved "text")
>                       *> (Wrapped <$> getPosition <*> Lexer.string))
>   where
>       cellXform text cell = cell { cellText = Just text }

The `cellStyleParser` parses a style reference in the cell block.  It produces a
function that maps a `Cell` to a `Cell` with that style reference.

> cellStyleParser :: Parser ((Cell g) -> (Cell g))
> cellStyleParser =
>       cellXform <$> (    (reserved "style")
>                       *> (Wrapped <$> getPosition <*> identifier))
>   where
>       cellXform style cell = cell { cellStyleRef = Just style }

The `cellPropertyParser` parses a single property from the cell block.  It
produces a function that maps a `Cell` to a `Cell` with that property.  This
parser tries each property parser in order.

> cellPropertyParser :: Parser ((Cell g) -> (Cell g))
> cellPropertyParser =
>       try cellTextParser
>   <|> cellStyleParser

The `cellPropertiesParser` combines all of the cell properties into a single
function mapping a `Cell` to a `Cell` with all of the properties applied in
sequence.

> cellPropertiesParser :: Parser ((Cell g) -> (Cell g))
> cellPropertiesParser =
>       cellPropertyFold <$> (many cellPropertyParser)
>   where
>       cellPropertyFold props cell =
>           foldl applyProp cell props
>       applyProp cell prop = prop cell

The `boxCellParser` parses a box cell with an optional label.

> boxCellParser :: Parser ((Lane g) -> (Lane g))
> boxCellParser =
>   laneCellXformer
>       <$> cellLabelParser
>       <*> (Wrapped <$> getPosition
>                    <*> (createBoxCell <$> (  (reserved "box")
>                                            *>(braces cellPropertiesParser))))
>   where
>       createBoxCell props =
>           props $ Cell Nothing Nothing Nothing Nothing Nothing CellTypeBox
>       laneCellXformer xform cell =
>           addCellToLane cell xform

The `squareBoxCellParser` parses a square box cell with an optional label.

> squareBoxCellParser :: Parser ((Lane g) -> (Lane g))
> squareBoxCellParser =
>   laneCellXformer
>       <$> cellLabelParser
>       <*> (Wrapped <$> getPosition
>                    <*> (createSquareCell <$> (  (reserved "square_box")
>                                           *>(braces cellPropertiesParser))))
>   where
>       createSquareCell props =
>           props $ Cell Nothing Nothing Nothing Nothing Nothing
>                        CellTypeSquareBox
>       laneCellXformer xform cell =
>           addCellToLane cell xform

The `diamondCellParser` parses a diamond cell with an optional label.

> diamondCellParser :: Parser ((Lane g) -> (Lane g))
> diamondCellParser =
>   laneCellXformer
>       <$> cellLabelParser
>       <*> (Wrapped <$> getPosition
>                    <*> (createDiamondCell <$> (  (reserved "diamond")
>                                            *>(braces cellPropertiesParser))))
>   where
>       createDiamondCell props =
>           props $ Cell Nothing Nothing Nothing Nothing Nothing CellTypeDiamond
>       laneCellXformer xform cell =
>           addCellToLane cell xform

The `cellParser` combines the `ellipsisCellParser`, `emptyCellParser`,
`boxCellParser`, and `diamondCellParser` into a single parser that parses a
`Cell` matching one of these types.  It returns a function that maps a `Lane`
onto a `Lane` with that cell.

> cellParser :: Parser ((Lane g) -> (Lane g))
> cellParser =
>       try ellipsisCellParser
>   <|> try emptyCellParser
>   <|> try boxCellParser
>   <|> try squareBoxCellParser
>   <|> diamondCellParser

The `lanePropertyParser` parses one of any of the properties supported by a
lane, returning a function that maps a `Lane` onto a `Lane` with the given
property.

> lanePropertyParser :: Parser ((Lane g) -> (Lane g))
> lanePropertyParser =
>       try cellParser
>   <|> laneStyleParser

The `lanePropertiesParser` parses all of the properties in a given lane,
returning a function that maps a `Lane` into a `Lane` that contains all of these
properties.

> lanePropertiesParser :: Parser ((Lane g) -> (Lane g))
> lanePropertiesParser =
>       lanePropertyFold <$> (many lanePropertyParser)
>   where
>       lanePropertyFold props lane =
>           foldl applyProp lane props
>       applyProp lane prop = prop lane

The `parseLane` function parses a top-level lane definition and returns a
`Wrapped Lane` populated with this definition.

> parseLane :: Parser (Wrapped (Lane g))
> parseLane =
>       Wrapped <$> getPosition
>               <*> (createLane <$> (reserved "lane" >> laneString)
>                               <*> (braces lanePropertiesParser))
>   where
>       laneString = Wrapped <$> getPosition <*> Lexer.string
>       createLane name props = props $ emptyLane name

Complete Parser
---------------

Each of the previous top-level definitions are combined to form a complete
parser that can parse an entire swim lanes document.  The `parseSwimLanes`
function parses a complete document by applying each of the top level definition
parsers.  To capture these applications, we create some helper parsers that
transform these top-level definition parsers into functions mapping a
`SwimLanes` structure into a `SwimLanes` structure with the given top-level
definition.

The first helper is `parseLaneHelper`.  This method transforms `parseLane` into
a `SwimLanes -> SwimLanes` mapping function.

> parseLaneHelper :: Parser ((SwimLanes g) -> (SwimLanes g))
> parseLaneHelper =
>       laneMappingXform <$> parseLane
>   where
>       laneMappingXform lane swim =
>           swim { swimLanes = (swimLanes swim) ++ [lane] }

The next helper is `parsePathHelper`.  This method transforms `parsePath` into a
`SwimLanes -> SwimLanes` mapping function.

> parsePathHelper :: Parser ((SwimLanes g) -> (SwimLanes g))
> parsePathHelper =
>       pathMappingXform <$> parsePath
>   where
>       pathMappingXform path swim =
>           swim { swimLanePaths = (swimLanePaths swim) ++ [path] }

The next helper is `parseStyleHelper`.  This method transforms `parseStyle` into
a `SwimLanes -> SwimLanes` mapping function.

> parseStyleHelper :: Parser ((SwimLanes g) -> (SwimLanes g))
> parseStyleHelper =
>       styleMappingXform <$> parseStyle
>   where
>       styleMappingXform style swim =
>           swim { swimLaneStyles =
>                     Map.insert (wrappedValue $ styleName $ wrappedValue style)
>                                style (swimLaneStyles swim) }

We can use these helpers to build a `parseTopLevelDefinition` parser, which
parses a top-level definition and returns a function that maps a `SwimLanes` to
a `SwimLanes` with that top-level definition.

> parseTopLevelDefinition :: Parser ((SwimLanes g) -> (SwimLanes g))
> parseTopLevelDefinition =
>       try parseLaneHelper
>   <|> try parsePathHelper
>   <|> parseStyleHelper

We can then build a `parseTopLevelDefinitions` parser, which repeatedly applies
the `parseTopLevelDefinition` parser, and folds all of the results into a single
function that maps a `SwimLanes` to a `SwimLanes` with each of the top level
definitions in it.

> parseTopLevelDefinitions :: Parser ((SwimLanes g) -> (SwimLanes g))
> parseTopLevelDefinitions =
>       tldFold <$> (many parseTopLevelDefinition)
>   where
>       tldFold props swim =
>           foldl applyProp swim props
>       applyProp swim prop = prop swim

Finally, we can build the `parseSwimLanes` parser, which parses a valid
`SwimLanes` document.

> parseSwimLanes :: Parser (SwimLanes g)
> parseSwimLanes =
>       createSwimLanes <$> (   reserved "swimlanes"
>                            >> braces parseTopLevelDefinitions)
>   where
>       createSwimLanes tlds =
>           tlds $ SwimLanes Map.empty [] [] Map.empty
