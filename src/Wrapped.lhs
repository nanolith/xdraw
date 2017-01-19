Wrapped Data Type
=================

This module describes the wrapped data type which is used to wrap a value with
position information.

> module Wrapped (
>       Wrapped (Wrapped, wrappedPosition, wrappedValue), unwrap
> ) where

We use the Parsec module for an opaque definition of `SourcePos`.

> import Text.Parsec (SourcePos)

Wrapped
-------

The `Wrapped` data type wraps parsed data with positional information uncovered
during parsing.  This allows us to build meaningful error messages during
semantic analysis.

The `wrappedValue` and `wrappedPosition` methods can be used to extract just the
value or position.

> data Wrapped a = Wrapped {
>                       wrappedPosition :: SourcePos,
>                       wrappedValue :: a
>                  } deriving (Eq, Show)

The `unwrap` helper function is a convenience method that decomposes a `Wrapped`
value into a tuple.

> unwrap :: Wrapped a -> (SourcePos, a)
> unwrap (Wrapped pos val) = (pos, val)
