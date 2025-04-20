{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module EulerGrid
  ( EulerGrid(..),
    EulerGridCoord(..),
    PlanarCoord(..),
    keyCorners
  ) where

import qualified PlanarGeometry as PG (Vect2, makeVect2, Line(..), intersect)
import Grid (Coordinates(..), GridCoordinates, Grid(..))

import Data.Maybe (catMaybes)

infixl 6 +@@
infixl 6 -@@
data EulerGridCoord = EulerGridCoord Int Int Int
eZero :: EulerGridCoord
eZero = EulerGridCoord 0 0 0
(+@@) :: EulerGridCoord -> EulerGridCoord -> EulerGridCoord
(EulerGridCoord o1 q1 g1) +@@ (EulerGridCoord o2 q2 g2) = (EulerGridCoord (o1 + o2) (q1 + q2) (g1 + g2))
(-@@) :: EulerGridCoord -> EulerGridCoord -> EulerGridCoord
(EulerGridCoord o1 q1 g1) -@@ (EulerGridCoord o2 q2 g2) = (EulerGridCoord (o1 - o2) (q1 - q2) (g1 - g2))


instance GridCoordinates EulerGridCoord


data PlanarCoord = PlanarCoord Double Double

instance Coordinates PlanarCoord Double where
  coord (a, b) = PlanarCoord a b
  coordfromTuple (PlanarCoord a b) = (a, b)
  cx (PlanarCoord a b) = a
  cy (PlanarCoord a b) = b
  x • (PlanarCoord a b) = PlanarCoord (x * a) (x * b)
  asTuple (PlanarCoord a b) = (a, b)
  zero = PlanarCoord 0 0
  (PlanarCoord a b) +# (PlanarCoord c d) = PlanarCoord (a+c) (b+d)
  (PlanarCoord a b) -# (PlanarCoord c d) = PlanarCoord (a-c) (b-d)
  llcoord (PlanarCoord a b) (PlanarCoord c d) = PlanarCoord (min a c) (min b d)
  rucoord (PlanarCoord a b) (PlanarCoord c d) = PlanarCoord (max a c) (max b d)


-- parentframe takes a list of coordinate pairs --  each interpreted as a rectangle -- and returns the coordinate pair representing the smallest rectangle containing the given rectangles.
parentframehelper :: (RealFloat a, Coordinates c a) => [(c,c)] -> (c,c) -> (c,c)
parentframehelper ((ll,ru):ces) (cll,cru) | null ces     = (cll,cru)
                                          | otherwise    = parentframehelper ces (llcoord ll cll,rucoord ru cru)

parentframe :: (RealFloat a, Coordinates c a) => [(c,c)] -> (c,c)
parentframe (ce:ces) = parentframehelper ces ce

-- Euler's Tone net
-- The data is interpreted as follows
-- EulerGrid <vector for octave step> <vector for fifth step> <vector for third step> <List Indizes representing octave coordinates for keys in net> <... fifth ...> <thirds>
-- For example: "Eulergrid (1,0) (0.6,0.3) (0.35,-0.2) [(-4)..4] [(-3)..3] [(-1)..1]"
-- for a keyboard ranging over 8 octaves, 6 fifth and 2 (major) thirds
data EulerGrid = EulerGrid PlanarCoord PlanarCoord PlanarCoord [Int] [Int] [Int]

instance Grid EulerGrid EulerGridCoord EulerGridCoord PlanarCoord Double where
  ingrid2 (EulerGrid oktave quinte gterz oktRange quintRange gterzRange) makeobject
    = [makeobject ((fromIntegral o) • oktave +# (fromIntegral q) • quinte +# (fromIntegral g) • gterz, EulerGridCoord o q g)
      | o <- oktRange, q <- quintRange, g <- gterzRange]
  frame (EulerGrid oktave quinte gterz oktRange quintRange gterzRange) makeobjectframe
    = parentframe [makeobjectframe ((fromIntegral o) • oktave +# (fromIntegral q) • quinte +# (fromIntegral g) • gterz, EulerGridCoord o q g)
                  | o <- oktRange, q <- quintRange, g <- gterzRange]
  pos (EulerGrid oktave quinte gterz oktRange quintRange qterzRange ) (EulerGridCoord o q g)
    = (fromIntegral o) • oktave +# (fromIntegral q) • quinte +# (fromIntegral g) • gterz

--  pos EulerGrid oktave quinte gterz x y z = Coordinate ((cx oktave) * x + (cx quinte) * y + (cx gterz) * z,(cy oktave) * x, (cy quinte) * y, (cy gterz) * z)

tn_CD =     EulerGridCoord (-1) 2 0
tn_CCisM =  EulerGridCoord (-2) 3 1
tn_CHis = EulerGridCoord (-3) 4 2
tn_CCesM =  EulerGridCoord 0 1 (-2)
tn_CH =     EulerGridCoord (-1) 1 1
tn_CB =     EulerGridCoord 1 (-2) 0
tn_CCesP =  EulerGridCoord 2 (-3) (-1)
tn_CDeses = EulerGridCoord 3 (-4) (-2)
tn_CCisP =  EulerGridCoord 0 (-1) 2
tn_CDes  =  EulerGridCoord 1 (-1) (-1)

tn_EFis  =  tn_CD
tn_EFesM  =  EulerGridCoord 1 0 (-3)
tn_EEsM  =  tn_CCesM
tn_EDis  =  tn_CH
tn_EEses =  EulerGridCoord 2 (-2) (-3)
tn_EEsP   =  tn_CCesP
tn_EFesP  = EulerGridCoord 3 (-4) (-2)
tn_EF    =  tn_CDes

tn_GisBes = EulerGridCoord 2 (-1) (-4)
tn_GisAsM  = tn_EFesM
tn_GisG   = tn_CCesM
tn_GisGes = tn_EEses
tn_GisAses = EulerGridCoord 3 (-3) (-4)
tn_GisAsP = tn_CDeses
tn_GisA   = tn_CDes

-- argument is level
environ :: Int -> [EulerGridCoord]
environ 0 = [ tn_CCisM, tn_CHis, tn_CCesM, tn_CH, tn_CCesP, tn_CDeses, tn_CCisP, tn_CDes]
environ 1 = [ tn_EFesM, tn_EEsM, tn_EDis, tn_EEsP, tn_EFesP, tn_EF]
environ 2 = [ tn_GisAsM, tn_GisG, tn_GisAses, tn_GisAsP, tn_GisA]
environ (l) =
  if l < (-2) || l > 2 then
    environ 0
  else
    map (\(EulerGridCoord a b c) -> EulerGridCoord (-a) (-b) (-c)) $ environ (-l)

{-
-- old versions
environ (-2) = [ EulerGridCoord (-2) 1 4, EulerGridCoord (-1) 0 3, EulerGridCoord (-1) 1 2,                EulerGridCoord (-2) 2 3, EulerGridCoord (-3) 3 4, EulerGridCoord (-1) 1 1]
environ 2    = [ EulerGridCoord 2 (-1) (-4), EulerGridCoord 1 0 (-3), EulerGridCoord 1 (-1) (-2),            EulerGridCoord 2 (-2) (-3), EulerGridCoord 3 (-3) (-4), EulerGridCoord 1 (-1) (-1)]
-}

-- arguments: Point1, Point2, weight of first (1- priority of other)
borderLine :: PlanarCoord -> PlanarCoord -> Int -> Int -> PG.Line
borderLine (PlanarCoord x1 y1) (PlanarCoord x2 y2) first_level second_level = PG.LineThroughPointInDirection middle direction
  where
    middle = PG.makeVect2 ((x1 + x2) / 2) ((first_weight * y1 + (1 - first_weight) * y2))
    direction = PG.makeVect2 (- ((y2 - y1))) (vert_factor * (x2 - x1))
    first_weight                            = rankToWeight diff_abs_level
    rankToWeight r  | r == 2                = 0.5
                    | r == 1                = 0.5
                    | r == 0                = 0.5
                    | r == (-1)             = (1 - (rankToWeight 1))
                    | r == (-2)             = (1 - (rankToWeight 2))
                    | otherwise             = 0.5
    vert_factor     | sal == 0 && gal == 0  = 32
                    | sal == 0 && gal == 1  = 32
                    | sal == 0 && gal == 2  = 32
                    | sal == 1 && gal == 1  = 32
                    | sal == 1 && gal == 2  = 32
                    | sal == 2 && gal == 2  = 32
                    | otherwise             = 32
    sal                                     = minimum [(abs first_level), (abs second_level)]
    gal                                     = maximum [(abs first_level), (abs second_level)]
    diff_abs_level                          = ((abs first_level) - (abs second_level))


-- arguments: Grid, Level(thirds)
keyShapeLines :: EulerGrid -> Int -> [PG.Line]
keyShapeLines eg@(EulerGrid vo vq vg ro rq rg) level
  = [ borderLine zero ((pos eg (relegcoord +@@ egcoord_center)) -# (pos eg egcoord_center)) level (getabsg relegcoord) | relegcoord <- environ level ]
  where
    egcoord_center = EulerGridCoord 0 0 level
    getabsg (EulerGridCoord o q g) = level + g

-- arguments: Grid, Level(thirds)
keyCorners :: EulerGrid -> Int -> [PG.Vect2]
keyCorners eg level =
  let
    lineList = keyShapeLines eg level
    lineListLen = length lineList
  in
    catMaybes [ PG.intersect (lineList!!(mod n lineListLen)) (lineList!!(mod (n+1) lineListLen))
              | n <- [0..lineListLen] ]
