module Types where

import Data.Word (Word8)
import Text.Printf (printf)

data Color = RGB Word8 Word8 Word8 deriving (Show,Eq)

getColorCode :: Color -> String
getColorCode (RGB r g b) = printf ("#" ++ concat (replicate 3 "%02x")) r g b

data PlaneGeoComputer scalar line
  = PlaneGeoComputer
  { pgcLineThroughPointInDirection
    :: Coordinates scalar
    -> Coordinates scalar
    -> line
  , pgcIntersectLines :: line -> line -> Maybe (Coordinates scalar)
  }

type Coordinates a = (a, a)

scale :: RealFloat a => a -> Coordinates a -> Coordinates a
scale t (x, y) = (t*x, t*y)

llcoord, rucoord
  :: RealFloat a
  => Coordinates a
  -> Coordinates a
  -> Coordinates a
llcoord (a,b) (c,d) = (min a c, min b d)
rucoord (a, b) (c, d) = (max a c, max b d)


infixl 6 +# 
(+#) :: RealFloat a => Coordinates a -> Coordinates a -> Coordinates a
(+#) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

infixl 6 -# 
(-#) :: RealFloat a => Coordinates a -> Coordinates a -> Coordinates a
(-#) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
