{-# LANGUAGE FlexibleContexts #-}

module PlainGeometry
  ( Vect2,
    makeVect2,
    toPair,
    Line(..),
    intersect ) where

import Numeric.LinearAlgebra.Data (Vector, fromList, toList, Matrix, flatten, fromRows, fromLists, cols)
import Numeric.LinearAlgebra (nullspace, linearSolve)
import Control.Monad (join)

type Vect2 = Vector Double

makeVect2 :: Double -> Double -> Vect2
makeVect2 a b = fromList [a,b]

toPair :: Vect2 -> (Double, Double)
toPair v = let [x,y] = toList v in (x,y)

data Line = LineThroughPoints Vect2 Vect2
          | LineThroughPointInDirection Vect2 Vect2
          | LineByEquation Double Double Double


instance Show Line where
  show (LineThroughPoints v1 v2) = "lineThrough(" ++ show v1 ++ ";" ++ show v2 ++ ")"
  show (LineThroughPointInDirection v1 v2) = "line((" ++ show v1 ++ ") + " ++ "t * (" ++ show v2 ++ "))"
  show (LineByEquation a b c) = "line( " ++ show a ++ " * x + " ++ show b ++ " * y = " ++ show c ++ ")"

lineEquationFromCoefficientSpaceBasis :: Matrix Double -> Maybe Line
lineEquationFromCoefficientSpaceBasis m = case cols m of
                                            1 -> let v = toList $ flatten m in
                                                   case length v of
                                                     3 -> Just $ LineByEquation (v!!0) (v!!1) (v!!2)
                                                     _ -> Nothing
                                            _ -> Nothing

getLineEquation :: Line -> Maybe Line
getLineEquation line@(LineByEquation _ _ _) = Just line
getLineEquation (LineThroughPoints v1 v2) =
  lineEquationFromCoefficientSpaceBasis . nullspace . fromRows $ map appendMinusOne [v1, v2]
  where
    appendMinusOne v = fromList ((toList v) Prelude.<> [-1])
getLineEquation (LineThroughPointInDirection v1 v2) = getLineEquation (LineThroughPoints v1 (v1 + v2))


intersect :: Line -> Line -> Maybe Vect2
intersect (LineByEquation a1 b1 c1) (LineByEquation a2 b2 c2) =
  flatten <$> (linearSolve (fromLists [[a1, b1], [a2, b2]]) $ fromLists [[c1],[c2]])
intersect l m = let [le, me] = (map getLineEquation) $ [l, m] in
                  join $ ((Just intersect) <*> le) <*> me

{--
intersect :: Line -> Line -> Maybe PlainCoord
intersect
  (LineThroughPoints (PlainCoord ax1 ay1) (PlainCoord ax2 ay2))
  (LineThroughPoints (PlainCoord bx1 by1) (PlainCoord bx2 by2))
  = let solution = 
          where nullSpace a = snd $ rightSV a
--}
