Lexer for xdraw
===============

This source file defines the lexical analyzer used by all xdraw language front
ends to tokenize input.

> module Lexer where

We use the String parser from Parsec.

> import Text.Parsec
> import Text.Parsec.String (Parser)

We build up a decent first pass language definition using `emptyDef`.  The
Parsec language definitions handle basics such as identifiers, comments, and
numbers.  Parsec frees us from having to deal with this minutiae.

> import Text.Parsec.Language (emptyDef)

Finally, we take advantage of the built-in Parsec Tokenizer to manage most of
the tokenization automagically.

> import qualified Text.Parsec.Token as Tok

Let's define some parameters for our token parser.  We will define reserved
words, comments, identifiers, and whether our language is case sensitive.

> lexer :: Tok.TokenParser ()
> lexer = Tok.makeTokenParser style
>   where
>       ops = []
>       names = []
>       style = emptyDef {
>                   Tok.commentStart     = "/*"
>                 , Tok.commentEnd       = "*/"
>                 , Tok.commentLine      = "//"
>                 , Tok.nestedComments   = True
>                 , Tok.identStart       = letter <|> oneOf "_"
>                 , Tok.identLetter      = alphaNum <|> oneOf "_"
>                 , Tok.reservedOpNames  = ops
>                 , Tok.reservedNames    = names
>                 , Tok.caseSensitive    = True
>               }

Tokenizer Helpers
=================

We expose a few helpers from the above lexer definition that we will use in our
parser.

Value Types
-----------

Our language handles integers, floating point numbers, hexadecimal numbers, and
strings.

This definition handles integers.

> integer :: Parser Integer
> integer = Tok.integer lexer

The `int` parser converts the integer value into an `Int`.

> int :: Parser Int
> int = fromIntegral <$> integer

This definition handles floats.  First we try parsing a float, and if that
fails, we will try parsing an integer value.  Since Parsec doesn't support
negative floats, we also add some logic for dealing with negative numbers.

> float :: Parser Double
> float =
>           --first try negative float.  Hack to work around lack of negative
>           --float support in Parsec
>           (try $ (0-) <$> (Tok.reservedOp lexer "-" >> Tok.float lexer))
>           --then try normal float
>       <|> (try $ Tok.float lexer)
>           --then try integer value
>       <|> (fromIntegral <$> integer)

This definition handles hexadecimal numbers.

> hexadecimal :: Parser Integer
> hexadecimal = read <$>
>                   ((++) <$> (Tok.symbol lexer "0x") <*> (many1 hexDigit))

Strings are parsed using the `string` combinator.

> string :: Parser String
> string = Tok.stringLiteral lexer

Structural Components
---------------------

We need certain functions to denote structure, such as braces, parentheses, and
commas.  The following definitions provide these structures.

Curly braces are handled by the braces combinator.  The braces combinator
expects a parser combinator as an argument that represents what is inside of the
braces.

> braces :: Parser a -> Parser a
> braces = Tok.braces lexer

Square brackets are handled by the `brackets` combinator.

> brackets :: Parser a -> Parser a
> brackets = Tok.brackets lexer

The `parens` combinator allows us to wrap an inner combinator in parentheses.

> parens :: Parser a -> Parser a
> parens = Tok.parens lexer

The `comma` combinator captures a comma.

> comma :: Parser String
> comma = Tok.comma lexer

The `commaSep` combinator lets us describe elements that are separated by
commas.  The result is a list of elements.

> commaSep :: Parser a -> Parser [a]
> commaSep = Tok.commaSep lexer

The `commaSep1` combinator is like `commaSep`, but requires at least one value.

> commaSep1 :: Parser a -> Parser [a]
> commaSep1 = Tok.commaSep1 lexer

The `dot` parser matches a dot and consumes any whitespace up to the dot.

> dot :: Parser String
> dot = Tok.dot lexer

The `semi` parser matches a semicolon and any whitespace up to the semicolon.

> semi :: Parser String
> semi = Tok.semi lexer

The `semiSep` parser parses a sequence separated by semicolons.

> semiSep :: Parser a -> Parser [a]
> semiSep = Tok.semiSep lexer

The `semiEnd` parser parses a sequence ended by semicolons.

> semiEnd :: Parser a -> Parser [a]
> semiEnd subParse = subParse `endBy` (Tok.semi lexer)

The equals parser parses an equals sign.

> equals :: Parser String
> equals = Tok.symbol lexer "="

The colon parser parses a colon.

> colon :: Parser String
> colon = Tok.symbol lexer ":"

Identifiers and Reserved Words
==============================

We also need to consume identifiers and reserved words.  The following two
parsers help us to do that.

Identifiers are handled by the `identifier` parser, which parses an identifier
using the lexer's rules.

> identifier :: Parser String
> identifier = Tok.identifier lexer

For this parser, we pass in the reserved word that we expect, and we get back a
parser that consumes this word.

> reserved :: String -> Parser ()
> reserved = Tok.reserved lexer

For this parser, we pass in the reserved operator we expect, and we get back a
parser that consumes this operator.

> reservedOp :: String -> Parser ()
> reservedOp = Tok.reservedOp lexer

Whitespace and Comments
=======================

The contents combinator skips over whitespace at the beginning of a compilation
unit and reads all of the way until eof.  Since we are using a pre-defined
lexer, this combinator also skips over whitespace and comments.

> contents :: Parser a -> Parser a
> contents parser = Tok.whiteSpace lexer >> parser >>= skipEof
>       where
>           skipEof x = eof >> return x
