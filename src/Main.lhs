xdraw Utility
=============

This module describes the xdraw command-line utility.

> module Main where

We use `Parsec` to kick off parsing.

> import Text.Parsec

We use `Text.Printf` for the `perror` method, which prints an error message and
raises an error.

> import Text.Printf

We use the `Diagrams` package for drawing pictures.

> import Diagrams.Prelude hiding (Path, Wrapped, pathVertices)
> import Diagrams.Backend.Postscript.CmdLine

We import our custom swim lanes parsing and analysis modules.

> import SwimLanes
> import SwimLanesGen

The first method we need is a `parseInputFile` method, which takes a `FilePath`
and returns an `IO (Diagram B)` which can then be drawn via `Diagrams`.

> parseInputFile :: FilePath -> IO (Diagram B)
> parseInputFile path = do
>       f <- readFile path
>       case (swimLaneAnalyze =<< parse parseSwimLanes path f) of
>           Left err -> perror $ show err
>           Right swim -> return (drawSwimLanes swim)

With this method, we can now create a proper `main` method that parses an input
file and turns it into an image.

> main :: IO ()
> main = mainWith parseInputFile
