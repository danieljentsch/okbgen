{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Grid (Coordinates(..), GridCoordinates, Grid(..)) where

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
