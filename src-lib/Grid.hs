{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Grid (GridCoordinates, Grid(..)) where

import Types

class GridCoordinates gc


-- | ingrid2: associates a list of objects to a grid and a function computing
--   an object from point in the plane and a parameter of type t
class
  (RealFloat a, GridCoordinates gc) =>
  Grid g t gc a | g -> a, g -> gc, g -> t where
  ingrid2 :: g -> ((Coordinates a, t) -> o) -> [o]
  frame
    :: g
    -> ((Coordinates a, t) -> (Coordinates a, Coordinates a))
    -> (Coordinates a, Coordinates a)
  pos :: g -> gc -> Coordinates a
