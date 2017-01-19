module ParserHelpers where

import Control.Applicative
import Control.Monad
import Data.Either
import Test.HUnit
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

--expect the given input to fail to parse
expectParseFail :: Parser a -> String -> String -> IO ()
expectParseFail parser input msg =
    assertBool msg (isLeft $ parse parser "testInput" input)

--expect the given input to succeed to parse
expectParseSuccess :: Parser a -> String -> String -> IO ()
expectParseSuccess parser input msg =
    assertBool msg (isRight $ parse parser "testInput" input)

--expect the given input to match the given AST
expectParseEquals :: (Eq a, Show a) => Parser a -> a -> String -> String
                                    -> IO ()
expectParseEquals parser ast input msg =
    assertEqual msg (Right ast) (parse parser "testInput" input)
