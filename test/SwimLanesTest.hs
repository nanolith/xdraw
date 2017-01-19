--Unit tests for SwimLanes
module SwimLanesTest (tests) where

import ParserHelpers
import SwimLanes
import Wrapped
import Control.Applicative
import Control.Monad
import Data.Either
import qualified Data.Map as Map
import Test.HUnit
import Text.Parsec hiding (string)
import Text.Parsec.Pos
import Text.Parsec.String (Parser)

--test that Wrapped accessors work
wrappedTest =
    TestList [
        TestCase $
            assertEqual "wrappedPosition returns position"
                pos (wrappedPosition wrappedVal),
        TestCase $
            assertEqual "wrappedValue returns value"
                val (wrappedValue wrappedVal),
        TestCase $
            assertEqual "wrappedValues can be compared"
                wrappedVal wrappedVal,
        TestCase $
            assertEqual "wrappedValues can be shown"
              (    "Wrapped {wrappedPosition = \"testInput\" (line 2, column 8)"
                ++ ", wrappedValue = 17}")
              (show wrappedVal)]
    where
        pos = newPos "testInput" 2 8
        val = 17
        wrappedVal = Wrapped pos val

--test that unwrap properly unwraps a wrapped value
unwrapTest =
    TestCase $
        assertEqual "Unwrap should unwrap values"
            (pos, val) (unwrap wrappedVal)
    where
        pos = newPos "testInput" 2 8
        val = 17
        wrappedVal = Wrapped pos val

--test the parseSegmentType function
parseSegmentTypeTest =
    TestList [
        TestCase $ testParse
            (wrapped STStraight) "straight" "parse straight segment.",
        TestCase $ testParse
            (wrapped (STNamed "fuzzy" Nothing)) "fuzzy" "parse named segment.",
        TestCase $ testParse
            (wrapped (STNamed "arrow" (Just "small"))) "arrow.small"
            "parsed named segment with subtype"]
    where
        testParse lhs rhs msg = expectParseEquals parseSegmentType lhs rhs msg
        wrapped val = Wrapped (newPos "testInput" 1 1) val

--test the parseLineStroke function
parseLineStrokeTest =
    TestList [
        TestCase $ testParse
            (wrapped (LSSolid 5)) "solid[5]" "parse solid (1).",
        TestCase $ testParse
            (wrapped (LSSolid 3)) "solid [3]" "parse solid (2).",
        TestCase $ testParse
            (wrapped (LSDashed 1 2 3)) "dashed[1,2,3]" "parse dashed (1).",
        TestCase $ testParse
            (wrapped (LSDashed 4 5 6)) "dashed [4,5,6]" "parse dashed (2).",
        TestCase $ testParse
            (wrapped (LSDashed 7 8 9)) "dashed[7, 8, 9 ]" "parse dashed (3).",
        TestCase $ testParse
            (wrapped (LSNamed "custom" 7)) "custom[7]" "parse named (1).",
        TestCase $ testParse
            (wrapped (LSNamed "froz" 21)) "froz [21]" "parse named (2)."]
    where
        testParse lhs rhs msg = expectParseEquals parseLineStroke lhs rhs msg
        wrapped val = Wrapped (newPos "testInput" 1 1) val

--test the parseColorType function
parseColorTypeTest =
    TestList [
        TestCase $ testParse
            (wrapped (ColorTypeName "black")) "color black" "parse color (1).",
        TestCase $ testParse
            (wrapped (ColorTypeName "blue")) "color blue" "parse color (2).",
        TestCase $ testParse
            (wrapped (ColorTypeName "red")) "color red" "parse color (3).",
        TestCase $ testParse
            (wrapped (ColorTypeName "green")) "color green" "parse color (4).",
        TestCase $ testParse
            (wrapped (ColorTypeRGB 0.1 0.1 0.1)) "colorRGB [0.1,0.1,0.1]"
            "parse colorRGB (1).",
        TestCase $ testParse
            (wrapped (ColorTypeRGB 0.25 0.5 1.0)) "colorRGB[0.25,0.5,1]"
            "parse colorRGB (2).",
        TestCase $ testParse
            (wrapped (ColorTypeRGB 0.75 0.25 0.5)) "colorRGB[0.75, 0.25, 0.5]"
            "parse colorRGB (3)."]
    where
        testParse lhs rhs msg = expectParseEquals parseColorType lhs rhs msg
        wrapped val = Wrapped (newPos "testInput" 1 1) val

--test the parseStyleElement function
parseStyleElementTest =
    TestList [
        TestCase $ testParse
            (wrapped (SEColor $ wrapped $ ColorTypeName "black"))
            "color black" "parse color (1).",
        TestCase $ testParse
            (wrapped (SEColor $ wrapped $ ColorTypeRGB 0.1 0.1 0.1))
            "colorRGB [0.1,0.1,0.1]" "parse color (2).",
        TestCase $ testParse
            (wrapped (SELine $ wc 6 $ LSSolid 7))
            "line solid[7]" "parse line stroke.",
        TestCase $ testParse
            (wrapped (SEBeginSegment $ wc 7 $ STStraight))
            "begin straight" "parse line segment (1).",
        TestCase $ testParse
            (wrapped (SEEndSegment $ wc 5 $ STNamed "arrow" Nothing))
            "end arrow" "parse line segment (2).",
        TestCase $ testParse
            (wrapped (SEEndSegment $ wc 5 $ STNamed "arrow" (Just "small")))
            "end arrow.small" "parse line segment (3)."]
    where
        testParse lhs rhs msg = expectParseEquals parseStyleElement lhs rhs msg
        wrapped val = wc 1 val
        wc col val = Wrapped (newPos "testInput" 1 col) val

--test the parseStyle parser
parseStyleTest =
    TestList [
        TestCase $ testParse
            (wrapped (Style (wc 7 "test") []))
            "style test {}" "empty style.",
        TestCase $ testParse
            (wrapped (Style (wc 7 "test") [
                wc 13 (SEColor $ wc 13 $ ColorTypeName "black")]))
            "style test {color black}" "style with one element.",
        TestCase $ testParse
            (wrapped (Style (wc 7 "test") [
                wc 13 (SEColor $ wc 13 $ ColorTypeName "black"),
                wc 25 (SELine $ wc 30 $ LSDashed 3 4 2)]))
            "style test {color black line dashed [3,4,2]}"
            "style with two elements."]
    where
        testParse lhs rhs msg = expectParseEquals parseStyle lhs rhs msg
        wrapped val = wc 1 val
        wc col val = Wrapped (newPos "testInput" 1 col) val

--test the parsePath parser
parsePathTest =
    TestList [
        TestCase $ testParse
            (wrapped ep)
            "path {}" "empty path.",
        TestCase $ testParse
            (wrapped ep {
                        pathStyleRef = Just $ wc 13 "foo" })
            "path {style foo}" "path with style.",
        TestCase $ testParse
            (wrapped ep {
                        pathVertices = [PVCellLabel $ wc  7 "x",
                                        PVCellLabel $ wc 11 "y",
                                        PVCellLabel $ wc 15 "z"]})
            "path {x-->y-->z}" "path with edges.",
        TestCase $ testParse
            (wrapped ep {
                        pathStyleRef = Just $ wc 13 "w",
                        pathVertices = [PVCellLabel $ wc 15 "x",
                                        PVCellLabel $ wc 19 "y",
                                        PVCellLabel $ wc 23 "z"]})
            "path {style w x-->y-->z}" "path with style and edges."]
    where
        ep :: SwimLanes.Path Int
        ep = emptyPath
        testParse lhs rhs msg =
          expectParseEquals (parsePath :: Parser (Wrapped (SwimLanes.Path Int)))
                            lhs rhs msg
        wrapped val = wc 1 val
        wc col val = Wrapped (newPos "testInput" 1 col) val

--test the parseLane parser
parseLaneTest =
    TestList [
        TestCase $ testParse
            (wrapped (el $ wc 6 "test lane"))
            "lane \"test lane\" {}" "empty lane.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneStyleRef = Just $ wc 25 $ "x" } )
            "lane \"test lane\" {style x}" "lane with style.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 19 $ Cell Nothing Nothing Nothing Nothing
                                          Nothing CellTypeEllipsis] } )
            "lane \"test lane\" {...}" "lane with ellipsis.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 22 $ Cell Nothing (Just $ wc 19 "X") Nothing
                                          Nothing Nothing CellTypeEllipsis] } )
            "lane \"test lane\" {X: ...}" "lane with labeled ellipsis.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 19 $ Cell Nothing Nothing Nothing Nothing
                                          Nothing CellTypeEmpty] } )
            "lane \"test lane\" {empty}" "lane with empty cell.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 22 $ Cell Nothing (Just $ wc 19 "X") Nothing
                                          Nothing Nothing CellTypeEmpty] } )
            "lane \"test lane\" {X: empty}" "lane with labeled empty cell.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 19 $ Cell Nothing Nothing Nothing Nothing
                                          Nothing CellTypeBox] } )
            "lane \"test lane\" {box {}}" "lane with empty box.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 22 $ Cell Nothing (Just $ wc 19 "X") Nothing
                                          Nothing Nothing CellTypeBox] } )
            "lane \"test lane\" {X: box {}}" "lane with labeled empty box.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 19 $ Cell Nothing Nothing Nothing Nothing
                                          (Just $ wc 30 "x") CellTypeBox] } )
            "lane \"test lane\" {box {style x}}" "lane with styled box.",
        TestCase $ testParse
            (wrapped $ (el $ wc 6 "test lane") {
                laneCells = [wc 19 $ Cell Nothing Nothing (Just $ wc 29 "x")
                                          Nothing Nothing CellTypeBox] } )
            "lane \"test lane\" {box {text \"x\"}}" "lane with text box."]
    where
        el :: Wrapped String -> Lane Int
        el = emptyLane
        testParse lhs rhs msg = expectParseEquals parseLane lhs rhs msg
        wrapped val = wc 1 val
        wc col val = Wrapped (newPos "testInput" 1 col) val

--test the parseSwimLanes parser
parseSwimLanesTest =
    TestList [
        TestCase $ testParse
            esl
            "swimlanes {}" "empty swim lane doc.",
        TestCase $ testParse
            esl {
                swimLaneStyles = Map.fromList [
                    ("x", wc 12 $ Style (wc 18 "x") [])]}
            "swimlanes {style x {}}" "swim lane doc with style.",
        TestCase $ testParse
            esl {
                swimLanes = [
                    wc 12 $ el (wc 17 "x")]}
            "swimlanes {lane \"x\" {}}" "swim lane doc with lane.",
        TestCase $ testParse
            esl {
                swimLanePaths = [
                    wc 12 $ ep]}
            "swimlanes {path {}}" "swim lane doc with path."]
    where
        esl :: SwimLanes Int
        esl = emptySwimLanes
        el :: Wrapped String -> Lane Int
        el = emptyLane
        ep :: SwimLanes.Path Int
        ep = emptyPath
        testParse lhs rhs msg = expectParseEquals parseSwimLanes lhs rhs msg
        wrapped val = wc 1 val
        wc col val = Wrapped (newPos "testInput" 1 col) val
        emptySwimLanes :: SwimLanes Int
        emptySwimLanes = SwimLanes Map.empty [] [] Map.empty

--list of unit tests for SwimLanes
tests = TestList [
            TestLabel "wrappedTest" wrappedTest,
            TestLabel "unwrapTest" unwrapTest,
            TestLabel "parseSegmentTypeTest" parseSegmentTypeTest,
            TestLabel "parseLineStrokeTest" parseLineStrokeTest,
            TestLabel "parseColorTypeTest" parseColorTypeTest,
            TestLabel "parseStyleElementTest" parseStyleElementTest,
            TestLabel "parseStyleTest" parseStyleTest,
            TestLabel "parsePathTest" parsePathTest,
            TestLabel "parseLaneTest" parseLaneTest,
            TestLabel "parseSwimLanesTest" parseSwimLanesTest]
