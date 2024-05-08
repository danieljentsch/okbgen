{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Grid where

{-
class Summable s where
  + :: s -> s -> s
-}

infixl 7 •
infixl 6 +#
infixl 6 -#
class RealFloat a => Coordinates c a | c -> a where
  coord :: (a, a) -> c
  coordfromTuple :: c -> (a, a)
  cx :: c -> a
  cy :: c -> a
  (•) :: a -> c -> c
  asTuple ::  c -> (a, a)
  zero :: c
  (+#) :: c -> c -> c
  (-#) :: c -> c -> c
  llcoord :: c -> c -> c
  rucoord :: c -> c -> c

class GridCoordinates gc


-- ingrid2: associates a list of objects to a grid and a function computing an object from point in the plain and a parameter of type t
class (RealFloat a, Coordinates c a, GridCoordinates gc) => Grid g t gc c a | g -> c a, g -> gc, g -> t where
  ingrid2 :: g -> ((c, t) -> o) -> [o]
  frame ::  g -> ((c, t) -> (c,c)) -> (c, c)
  pos :: g -> gc -> c


{-
class GridCoordinates

class (GridCoordinates c) => GridHalf h c | h -> c where


class (GridCoordinates c) => Newgrid n c | n -> c where

-- g - grid
-- t - type of 'types' of nodes (for example white_key :: t, black_key :: t)
-- c - coordinate type
-- a - RealFloat-type
-- A Grid re


class (GridCoordinates c, Newgrid n c) => Pattern p n c | p -> n c where
-}
