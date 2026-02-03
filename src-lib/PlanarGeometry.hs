{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : PlanarGeometry
Description : Provides data types for half-planes in the plane, or, equivalently, directed lines, and an intersection function.
Copyright   : (c) 2024, Daniel Jentsch
License     : MIT
Maintainer  : dev@danieljentsch.com
Stability   : experimental
Portability : Depends on LAPACK and BLAS.

This module defines the data type Line representing a lines in the plane with a direction.
Those are equivalent to half-planes.

This module uses `hmatrix` which relies on the LAPACK and BLAS libraries.  On Debian, installing the packages `liblapack-dev` and `libblas-dev` provides them.
-}


module PlanarGeometry (
  -- * functionality
  doublePgc,
  -- * Vectors in the plane
  Vect2,
  makeVect2,
  toPair,
  -- * Lines (with direction)
  Line(..),
  isLeftOf,
  intersect,
  intersectOrder
  ) where

import Numeric.LinearAlgebra.Data
  (Vector, fromList, toList, Matrix, flatten, fromRows, fromLists, cols)
import Numeric.LinearAlgebra (nullspace, linearSolve, scale, (<.>), (#>))
import Control.Monad (join)

import Types

doublePgc :: PlaneGeoComputer Double Line
doublePgc
  = PlaneGeoComputer
  { pgcLineThroughPointInDirection =
      (\pt dir -> LineThroughPointInDirection (fromPair pt) (fromPair dir))
  , pgcIntersectLines = (\a b -> fmap toPair $ intersect a b)
  }

-- epsilon
--  - a number is considered positive
--    only if it is numerically larger than epsilon
epsilon :: Double
epsilon = 0.00000001

clearSignum :: Integral a => Double -> Maybe a
clearSignum d = if d >= epsilon
           then Just 1
           else if d <= (-epsilon)
                then Just (-1)
                else Nothing
          
-- | Represents a vector in the plane.
type Vect2 = Vector Double

-- | Create a vector from two Double values.
makeVect2 :: Double -> Double -> Vect2
makeVect2 a b = fromList [a,b]


-- | Make a Vect2 from a pair of Double values.
fromPair :: (Double, Double) -> Vect2
fromPair (x, y) = makeVect2 x y

-- | Make a pair of Double values from a Vect2.
toPair :: Vect2 -> (Double, Double)
toPair v = let [x,y] = toList v in (x,y)

-- | Represents a line with direction or, equivalently, a half-plane.
data Line = LineThroughPoints Vect2 Vect2
          | LineThroughPointInDirection Vect2 Vect2
          | LineByEquation Double Double Double

instance Show Line where
  show (LineThroughPoints v1 v2) =
    "lineThrough(" ++ show v1 ++ ";" ++ show v2 ++ ")"
  show (LineThroughPointInDirection v1 v2) =
    "line((" ++ show v1 ++ ") + " ++ "t * (" ++ show v2 ++ "))"
  show (LineByEquation a b c) =
    "line( " ++ show a ++ " * x + " ++ show b ++ " * y = " ++ show c ++ ")"

-- util
getSingleVectorFromCoefficientSpaceBasis :: Matrix Double -> Maybe Vect2
getSingleVectorFromCoefficientSpaceBasis m = case cols m of
                                            1 -> let v = flatten m
                                                     vl = toList v in
                                                   case length vl of
                                                     3 -> Just v
                                                     _ -> Nothing
                                            _ -> Nothing

-- util
-- no checks performed(!):
lineEquationFromDirectedCoefficientVector :: Vector Double -> Line
lineEquationFromDirectedCoefficientVector v =
  let vl = toList v in LineByEquation (vl!!0) (vl!!1) (vl!!2)

-- util
rotateByPiByTwo :: Vect2 -> Vect2
rotateByPiByTwo = (fromLists [[0,-1],[1,0]] #>)

-- util
appendMinusOne :: Vect2 -> Vect2
appendMinusOne v = fromList ((toList v) Prelude.<> [-1])

-- get particular representation of line,
--   namely as solution set to an equation, if possible
getLineEquation :: Line -> Maybe Line
getLineEquation line@(LineByEquation _ _ _) = Just line
getLineEquation (LineThroughPoints v1 v2) = do
  let coefficientSpaceBasis =
        nullspace . fromRows $ map appendMinusOne [v1, v2]
  coefficientVector <-
    getSingleVectorFromCoefficientSpaceBasis coefficientSpaceBasis
  sig <- clearSignum $ appendMinusOne canonOrth <.> coefficientVector
  let directedCoefficientVector =
        Numeric.LinearAlgebra.scale (fromIntegral sig) coefficientVector
  pure (lineEquationFromDirectedCoefficientVector directedCoefficientVector)
  where
    canonOrth = rotateByPiByTwo (v2-v1)
getLineEquation (LineThroughPointInDirection v1 v2) =
  getLineEquation (LineThroughPoints v1 (v1 + v2))

-- Resulting vector is *not necessarily* of norm 1.
getCanonOrth :: Line -> Maybe Vect2
getCanonOrth l = do
  LineByEquation a b c <- getLineEquation l
  pure (makeVect2 a b)

isLeftOf :: Integral a => Vect2 -> Line -> Maybe a
isLeftOf v l = do
  LineByEquation a b c <- getLineEquation l
  clearSignum $ fromList [a, b, c] <.> (appendMinusOne v)

-- | Computes the intersection of two lines, if possible.
intersect :: Line -> Line -> Maybe Vect2
intersect (LineByEquation a1 b1 c1) (LineByEquation a2 b2 c2) =
  flatten <$>
  (linearSolve (fromLists [[a1, b1], [a2, b2]]) $ fromLists [[c1],[c2]])
intersect l m = let [le, me] = (map getLineEquation) $ [l, m] in
                  join $ ((Just intersect) <*> le) <*> me

-- | Computes the intersection of two (oriented) lines and a number whose
-- sign indicates whether at the intersection, going from the first to the
-- second line is a left turn (positive sign) or a right turn (negative
-- sign).
intersectOrder :: Integral a => Line -> Line -> Maybe (Vect2, a)
intersectOrder
  line1@(LineByEquation a1 b1 c1)
  line2@(LineByEquation a2 b2 c2) =
  do
    intersection <- flatten <$>
      (linearSolve (fromLists [[a1, b1], [a2, b2]]) $ fromLists [[c1],[c2]])
      -- ^ note: fromLists works row-wise
    canonOrth1 <- getCanonOrth line1
    canonOrth2 <- getCanonOrth line2
    orientation <-
      clearSignum ((rotateByPiByTwo $ canonOrth1) <.> (canonOrth2))
    pure (intersection, orientation)
intersectOrder l m = do
  le <- getLineEquation l
  me <- getLineEquation m
  intersectOrder le me

data Sense = Clockwise | CounterClockwise

data Side = Left | Right

data ToSense = ToSense Sense

data ToSide = ToSide Side

data OrientedLineRelation
  = OrientedIntersect Vect2 ToSense
  | CloseToAntiParallel Sense
  | CloseToCoParallel ToSide


{-
intersectOriented :: Line -> Line -> OrientedLineRelation
intersectOriented
  line1@(LineByEquation a1 b1 c1)
  line2@(LineByEquation a2 b2 c2) =
  do
    intersection <- flatten <$>
      (linearSolve (fromLists [[a1, b1], [a2, b2]]) $ fromLists [[c1],[c2]])
      -- ^ note: fromLists works row-wise
    canonOrth1 <- getCanonOrth line1
    canonOrth2 <- getCanonOrth line2
    orientation <- clearSignum
      ((rotateByPiByTwo $ canonOrth1) <.> (canonOrth2))
    pure (intersection, orientation)
-}

data Chain = Chain Line (Maybe (Vect2, Chain))

tryHookAtEnd :: ([Line], Chain, [Line]) -> ([Line], Chain, [Line])
tryHookAtEnd (line:lines, chain@(Chain head rest), misses) =
  case intersectOrder head line of
    Just (intersection, 1) -> case rest of
      Just (intersection', chain') -> case isLeftOf intersection' line of
        Just 1 -> (lines, Chain line (Just (intersection, chain )), misses)
        Just (-1) -> tryHookAtEnd (line:lines, chain', misses)
        Nothing -> (lines, chain, line:misses) -- questionable
      Nothing -> (lines, Chain line (Just (intersection, chain)), misses)
    Just (intersection, (-1)) -> (lines, chain, line:misses)




