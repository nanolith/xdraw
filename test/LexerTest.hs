--Unit tests for Lexer
module LexerTest (tests) where

import Lexer
import ParserHelpers
import Control.Applicative
import Control.Monad
import Data.Either
import Test.HUnit
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

--We should be able to parse ints
intTestPositive =
    TestList [
        TestCase $
            expectParseEquals int 21 "21" "Can parse positive ints.",
        TestCase $
            expectParseEquals int 0 "0" "Can parse zero.",
        TestCase $
            expectParseEquals int (-15) "-15" "Can parse negative ints."]

--We should not parse non-int values
intTestNegative =
    TestList [
        TestCase $ expectParseFail int ".56" "Can't parse decimal.",
        TestCase $ expectParseFail int "foo" "Can't parse non-numerics."]

--We should be able to parse floats
floatTestPositive =
    TestList [
        TestCase $
            expectParseEquals float 21 "21" "Positive int as float.",
        TestCase $
            expectParseEquals float 0 "0" "Zero int as float.",
        TestCase $
            expectParseEquals float (-15) "-15" "Negative int as float.",
        TestCase $
            expectParseEquals float 0.1 "0.1" "Float with decimal and prefix",
        TestCase $
            expectParseEquals float (-0.5) "-0.5" "Negative float.",
        TestCase $
            expectParseEquals float 100 "1e2" "Float with positive exponent",
        TestCase $
            expectParseEquals float 0.1 "1e-1" "Float with negative exponent"]

--We should not parse non-float values
floatTestNegative =
    TestList [
        TestCase $ expectParseFail float "A5C6" "Can't parse hex.",
        TestCase $ expectParseFail float "foo" "Can't parse non-numerics."]

--We should be able to parse hexadecimal numbers
hexadecimalTestPostive =
    TestList [
        TestCase $ expectParseEquals hexadecimal 0xa "0xa" "hex 0xa",
        TestCase $ expectParseEquals hexadecimal 0x0 "0x0" "hex 0x0",
        TestCase $ expectParseEquals hexadecimal 0x74c "0x74c" "hex 0x74c"]

--We should not parse non-hex numbers
hexadecimalTestNegative =
    TestList [
        TestCase $ expectParseFail hexadecimal "17" "Can't parse base 10 int.",
        TestCase $ expectParseFail hexadecimal "0.7" "Can't parse decimal.",
        TestCase $ expectParseFail hexadecimal "ab17" "Can't parse missing 0x."]

--We should be able to parse string literals
stringTestPositive =
    TestList [
        TestCase $
            expectParseEquals string "foo bar" "\"foo bar\"" "Simple string",
        TestCase $
            expectParseEquals string "abc\n" "\"abc\\n\"" "Escape sequences"]

--We should not parse non-strings or invalid strings
stringTestNegative =
    TestList [
        TestCase $
            expectParseFail string "abc" "Non-string literal.",
        TestCase $
            expectParseFail string "\"abc\\x\"" "Bad escape sequence.",
        TestCase $
            expectParseFail string "\"incomplete" "Incomplete string."]

--test braces
bracesTest =
    TestList [
        TestCase $
            expectParseEquals (braces int) 5 "{5}" "Int in braces.",
        TestCase $
            expectParseFail (braces int) "{5" "Missing end brace.",
        TestCase $
            expectParseFail (braces int) "5}" "Missing begin brace."]

--test brackets
bracketsTest =
    TestList [
        TestCase $
            expectParseEquals (brackets int) 5 "[5]" "Int in brackets.",
        TestCase $
            expectParseFail (brackets int) "[5" "Missing end bracket.",
        TestCase $
            expectParseFail (brackets int) "5]" "Missing begin bracket."]

--test parentheses
parensTest =
    TestList [
        TestCase $
            expectParseEquals (parens int) 5 "(5)" "Int in parentheses.",
        TestCase $
            expectParseFail (parens int) "(5" "Missing end parenthesis.",
        TestCase $
            expectParseFail (parens int) "5)" "Missing begin parenthesis."]

--test commaSep
commaSepTest =
    TestList [
        TestCase $
            expectParseEquals (commaSep int) [] "" "Empty list.",
        TestCase $
            expectParseEquals (commaSep int) [5] "5" "Single value.",
        TestCase $
            expectParseEquals (commaSep int) [5,6,7] "5,6,7" "commaSep ints."]

--test commaSep1
commaSep1Test =
    TestList [
        TestCase $
            expectParseEquals (commaSep1 int) [5] "5" "Single value.",
        TestCase $
            expectParseEquals (commaSep1 int) [5,6,7] "5,6,7" "commaSep ints.",
        TestCase $
            expectParseFail (commaSep1 int) "" "Empty fails for commaSep1."]

--test dot
dotTest =
    TestList [
        TestCase $
            expectParseEquals dot "." "." "Simple case.",
        TestCase $
            expectParseFail dot "71" "dot fails for non-dot input."]

--test comma
commaTest =
    TestList [
        TestCase $
            expectParseEquals comma "," "," "Simple case.",
        TestCase $
            expectParseFail comma "71" "comma fails for non-comma input."]

--test semicolon
semiTest =
    TestList [
        TestCase $
            expectParseEquals semi ";" ";" "Simple case.",
        TestCase $
            expectParseFail semi "71" "semi fails for non-semicolon input."]

--test semiSep
semiSepTest =
    TestList [
        TestCase $
            expectParseEquals (semiSep int) [] "" "Empty list.",
        TestCase $
            expectParseEquals (semiSep int) [5] "5" "Single value.",
        TestCase $
            expectParseEquals (semiSep int) [5,6,7] "5;6;7" "semiSep ints."]

--test semiEnd
semiEndTest =
    TestList [
        TestCase $
            expectParseEquals (semiEnd int) [] "" "Empty list.",
        TestCase $
            expectParseEquals (semiEnd int) [5] "5;" "Single value.",
        TestCase $
            expectParseEquals (semiEnd int) [5,6,7] "5;6;7;" "semiSep ints."]

--test equals
equalsTest =
    TestList [
        TestCase $
            expectParseEquals equals "=" "=" "Simple case.",
        TestCase $
            expectParseFail equals "71" "semi fails for non-equals input."]

--test colon
colonTest =
    TestList [
        TestCase $
            expectParseEquals colon ":" ":" "Simple case.",
        TestCase $
            expectParseFail colon "71" "semi fails for non-equals input."]

--Test identifier
identifierTest =
    TestList [
        TestCase $
            expectParseEquals identifier "abc" "abc" "Simple case.",
        TestCase $
            expectParseEquals identifier "abc123" "abc123" "Numerics.",
        TestCase $
            expectParseEquals identifier "abc_123" "abc_123" "Underscore.",
        TestCase $
            expectParseEquals identifier "_123" "_123" "Underscore prefix.",
        TestCase $
            expectParseFail identifier "71" "id must start with letter or _."]

--Test reserved
reservedTest =
    TestList [
        TestCase $
            expectParseSuccess (reserved "abc") "abc" "Simple case.",
        TestCase $
            expectParseSuccess (reserved "abc123") "abc123" "Numerics.",
        TestCase $
            expectParseSuccess (reserved "abc_123") "abc_123" "Underscore.",
        TestCase $
            expectParseSuccess (reserved "_123") "_123" "Underscore prefix.",
        TestCase $
            expectParseFail (reserved "xy") "71" "non-matching reserved."]

--Test reservedOp
reservedOpTest =
    TestList [
        TestCase $
            expectParseSuccess (reservedOp ",") "," "comma.",
        TestCase $
            expectParseSuccess (reserved "-->") "-->" "Arrow.",
        TestCase $
            expectParseSuccess (reserved "<--") "<--" "Back arrow.",
        TestCase $
            expectParseFail (reserved "-->") "," "non-matching reservedOp."]

--Test contents
contentsTest =
    TestList [
        TestCase $
            expectParseEquals (contents int) 5 "5" "Just content.",
        TestCase $
            expectParseEquals (contents int) 5 "  5" "Removes leading ws.",
        TestCase $
            expectParseEquals (contents int) 5 "5   " "Removes trailing ws.",
        TestCase $
            expectParseEquals (contents int) 5 "//123\n5" "Ignore comments 1.",
        TestCase $
            expectParseEquals (contents int) 5 "/*1*/5" "Ignore comments 2.",
        TestCase $
            expectParseEquals (contents int) 5 "5//123" "Ignore comments 3.",
        TestCase $
            expectParseEquals (contents int) 5 "5/*8*/" "Ignore comments 4.",
        TestCase $
            expectParseFail (contents int) "bad" "Fail on bad data."]

--list of unit tests for Lexer
tests = TestList [
            TestLabel "intTestPositive" intTestPositive,
            TestLabel "intTestNegative" intTestNegative,
            TestLabel "floatTestPositive" floatTestPositive,
            TestLabel "floatTestNegative" floatTestNegative,
            TestLabel "hexadecimalTestPostive" hexadecimalTestPostive,
            TestLabel "hexadecimalTestNegative" hexadecimalTestNegative,
            TestLabel "stringTestPositive" stringTestPositive,
            TestLabel "stringTestNegative" stringTestNegative,
            TestLabel "bracesTest" bracesTest,
            TestLabel "bracketsTest" bracketsTest,
            TestLabel "parensTest" parensTest,
            TestLabel "commaSepTest" commaSepTest,
            TestLabel "commaSep1Test" commaSep1Test,
            TestLabel "dotTest" dotTest,
            TestLabel "commaTest" commaTest,
            TestLabel "semiTest" semiTest,
            TestLabel "semiSepTest" semiSepTest,
            TestLabel "semiEndTest" semiEndTest,
            TestLabel "equalsTest" equalsTest,
            TestLabel "colonTest" colonTest,
            TestLabel "identifierTest" identifierTest,
            TestLabel "reservedTest" reservedTest,
            TestLabel "reservedOpTest" reservedOpTest,
            TestLabel "contentsTest" contentsTest]
