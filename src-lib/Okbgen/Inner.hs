{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Okbgen.Inner where

import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Data.Char (digitToInt, isHexDigit)
import GHC.Float (float2Double)
import Graphics.Svg (Element, (<<-))
import Data.Semigroup (stimesMonoid)
import Data.Text (Text)

import qualified Data.Text     as T
import qualified Data.Ix       as DI
import qualified Graphics.Svg  as Svg


import EulerGrid      (EulerGrid(..), EulerGridCoord(..), keyCorners)
import Grid           (ingrid2)
import PlanarGeometry (doublePgc)
import ToSvg
import Types


-- presentation related (html, colors, svg)

black :: Color
black = RGB 0 0 0

orBlack :: Maybe Color -> Color
orBlack (Just color) = color
orBlack Nothing = black

mkColorHTML :: String -> Maybe Color
mkColorHTML str =
  let
    rByte :: Char -> Char -> Maybe Word8
    rByte d1 d2 =
      if isHexDigit d1 && isHexDigit d2 then
        Just $ fromIntegral (digitToInt d1 * 16 + digitToInt d2)
      else
        Nothing
    fromHex :: Char -> Char -> Char -> Char -> Char -> Char -> Maybe Color
    fromHex r1 r2 g1 g2 b1 b2 =
      RGB <$> rByte r1 r2 <*> rByte g1 g2 <*> rByte b1 b2
  in
    case str of
      '#':r:g:b:[] -> fromHex r r g g b b
      '#':r1:r2:g1:g2:b1:b2:[] -> fromHex r1 r2 g1 g2 b1 b2
 
interColor :: RealFloat a => Color -> Color -> a -> Color
interColor c@(RGB cr cg cb) d@(RGB dr dg db) f
    | f <= 0    = c
    | f >= 1    = d
    | otherwise = RGB (wm cr dr) (wm cg dg) (wm cb db)
  where
    wm a b = round $ (1-f) * (fromIntegral a) + f * (fromIntegral b)

-- data Layout = LayoutNode

-- data LayoutNode = [];

cellsize, celltowidth, relativeborderwidth :: RealFloat a => a
cellsize = 30.0
celltowidth = 0.1
relativeborderwidth = 0.001


-- labels for keys.
tonename :: Int -> Int -> Int -> Text
tonename octaves quints gterzes = commata <> oktavetonename
  where
    commata
      | gterzes > 0      = stimesMonoid gterzes ","
      | gterzes == 0     = ""
      | gterzes < 0      = stimesMonoid (-gterzes) "\'"
    oktavetonename
      | whichoctave > 0  =
          tonenameroot 0 <> stimesMonoid whichoctave ("\'" :: Text)
      | whichoctave == 0 = tonenameroot 0
      | whichoctave < 0  =
        tonenameroot 1 <> stimesMonoid (-1 - whichoctave) ","
    whichoctave = (floor logdesttobase) :: Int
    tonenameroot cas     = makecase cas $ quintrow (quints + gterzes * 4)
    logdesttobase        =
      (log 2.0 * fromIntegral octaves
        + log (3.0 / 2.0) * fromIntegral quints
        + log (5.0 / 4.0) * fromIntegral gterzes)
      / log 2.0
    makecase cas txt
      | cas == 0 = (txt :: Text)
      | cas == 1 = T.toTitle (txt :: Text)
    quintrow n
      | n < (-8) =
          qrflat !! (mod (n + 1) 7)
          <> stimesMonoid ((-1) - div (n + 1) 7) ("es" :: Text)
      | DI.inRange (-8, -2) n = qrflat !! ((n + 1) + 7)
      | DI.inRange (-1, 5) n = qrbase !! (n + 1)
      | DI.inRange (6, 12) n = qrsharp !! ((n + 1) - 7)
      | 13 <= n =
          qrsharp !! (mod (n + 1) 7)
          <> stimesMonoid (div (n + 1) 7 - 1) ("is" :: Text)
    qrbase = ["f", "c", "g", "d", "a", "e", "h"] :: [Text]
    qrsharp = ["fis", "cis", "gis", "dis", "ais", "eis", "his"] :: [Text]
    qrflat = ["fes", "ces", "ges", "des", "as", "es", "b"] :: [Text]



-- Configuration Parameters

data OkbParam
  = OkbParam
  { opCanvasSize       :: (Float, Float)
  , opCanvasPxPerUnit  :: (Float, Float)
  , opOriginOnCanvas   :: (Float, Float)
  , opGridOctavesRange :: (Int, Int)
  , opGridFifthsRange  :: (Int, Int)
  , opGridThirdsRange  :: (Int, Int)
  , opKeyColors        :: (Int -> Color)
  }

data OkbParam' a
  = OkbParam'
  { opCanvasSize' :: (a, a),
    opCanvasPxPerUnit' :: (a, a),
    opOriginOnCanvas' :: (a, a),
    opGridOctavesRange' :: (Int, Int),
    opGridFifthsRange' :: (Int, Int),
    opGridThirdsRange' :: (Int, Int),
    opKeyColors' :: (Int -> Color)
  }

opDash :: OkbParam -> OkbParam' Float
opDash (OkbParam a b c d e f g) = (OkbParam' a b c d e f g)

opUndash :: OkbParam' Float -> OkbParam
opUndash (OkbParam' a b c d e f g) = (OkbParam a b c d e f g)

opFloat2Double :: OkbParam' Float -> OkbParam' Double
opFloat2Double (OkbParam' (a,b) (c,d) (e,f) g h i j) =
  OkbParam'
  (float2Double a, float2Double b)
  (float2Double c,float2Double d)
  (float2Double e, float2Double f)
  g
  h
  i
  j

instance Show OkbParam where
  show pars = show (rawParamDeconstructor pars)

instance (RealFloat a, Show a) => Show (OkbParam' a) where
  show pars = show (rawParamDeconstructor' pars)

-- standard values
stdColors :: Int -> String
stdColors (-2) = "#0f0e14"
stdColors (-1) = "#9db2c5"
stdColors    0 = "#c2c0b9"
stdColors    1 = "#c48569"
stdColors    2 = "#3f2a1e"
stdColors    _ = "#ffffff"

stdParams :: OkbParam
stdParams
  = OkbParam
  { opCanvasSize       = (1920, 1080)
  , opCanvasPxPerUnit  = ( 400,  400)
  , opOriginOnCanvas   = (1000,  540)
  , opGridOctavesRange = (  -8,    8)
  , opGridFifthsRange  = ( -10,   10)
  , opGridThirdsRange  = (  -2,    2)
  , opKeyColors = orBlack . mkColorHTML . stdColors
  }

-- may throw an error!  check arguments
rawParamBuilder :: [Float] -> [Int] -> [Color] -> OkbParam
rawParamBuilder [c1,c2,c3,c4,c5,c6] [g1,g2,g3,g4,g5,g6] colors =
  OkbParam (c1, c2) (c3, c4) (c5, c6) (g1, g2) (g3, g4) (g5, g6) colFun
  where
    colFun x | x >= g5 && x <= g6 = colors!!(x - g5)
             | otherwise          = colors!!(g6 - g5 + 1)

rawParamDeconstructor' :: RealFloat a => OkbParam' a -> ([a],[Int],[Color])
rawParamDeconstructor'
  (OkbParam' (c1, c2) (c3, c4) (c5, c6) (g1, g2) (g3, g4) (g5, g6) colFun) =
  ([c1,c2,c3,c4,c5,c6], [g1,g2,g3,g4,g5,g6], colors)
  where
    colors = map colFun [g5..(g6+1)]

rawParamDeconstructor :: OkbParam -> ([Float],[Int],[Color])
rawParamDeconstructor = rawParamDeconstructor' . opDash

-- TODO: Check if (a == 0) false implies that
--       (x / a) is not an error (for floats).
updateHeightKeepRatio oldWidth oldHeight newWidth
  | oldWidth == 0  = oldHeight
  | otherwise      = oldHeight * newWidth / oldWidth

-- try to center an integer range around center of given range;
--   if necessary, move slightly up
updateRange :: Int -> Int -> Int -> (Int, Int)
updateRange oldMin oldMax newDiff =
  let twiceUpper = oldMin + oldMax + newDiff
      twiceLower = oldMin + oldMax - newDiff in
  if even twiceUpper
  then (twiceUpper `div` 2, twiceLower `div` 2)
  else ((twiceUpper + 1) `div` 2, (twiceLower + 1) `div` 2)

sampleColorRange :: Int -> [Color] -> [Color]
sampleColorRange n rangePoints = case (n, rangePoints) of
  (n', _)  | n' <= 0  -> []
  (_, [])             -> replicate n black
  (_, [color])        -> replicate n color
  (1, (r:rps))        -> let [a,b,c] = replicate 3 rangePoints in b
  (_, _)              ->
    [rangePoints!!0]
    ++ [ interColor
         (rangePoints!!rpIndex)
         (rangePoints!!(rpIndex+1))
         fraction
       | i <- [1..(n-2)],
         let (rpIndex, fraction) =
               properFraction
               (fromIntegral (length rangePoints - 1)
                * fromIntegral i
                / (fromIntegral (n-1) :: Float) )
       ]
    ++ [rangePoints!!(n-1)]

tolerantParamBuilder :: OkbParam -> [Float] -> [Int] -> [String] -> OkbParam
tolerantParamBuilder defaultP canvasPs gridPs colorStrings =
  let (stdCanvasPs, stdGridPs, stdColors) = rawParamDeconstructor defaultP
      colors = catMaybes $ map mkColorHTML colorStrings
      -- update[what]
      --   step
      --   <remaining new parameters>
      --   <remaining std parameters>
      --   = <resulting parameters>
      -- where
      -- [what] = Canvas
      -- steps:
      --   (0 canvas size)
      --   (1 (canvas pixel / unit of image))
      --   (2 position of image origin on canvas)
      updateCanvas step (n:m:ns) (d:e:ds) =
        (if step < 3 then  n:m:(updateCanvas (step+1) ns ds) else [])
      -- | updating canvas size
      updateCanvas 0    [n]    (d:e:ds) = n:(updateHeightKeepRatio d e n):ds
      -- | updating (canvas pixel / unit of image)
      updateCanvas 1    [n]    (d:e:ds) = n:(updateHeightKeepRatio d e n):ds 
      -- | updating position of image origin on canvas
      updateCanvas 2    [n]      (d:e:ds) = [n,e] 
      updateCanvas step [] allds@(d:e:ds) = allds
      updateCanvas _    _              ds = ds -- should never occur
      -- [what] = Grid
      -- steps:
      --   (0 octave range)
      --   (1 fifths range)
      --   (2 position of image origin on canvas)
      updateGrid :: Int -> [Int] -> [Int] -> [Int]
      updateGrid step newP defaultP = case (step, newP, defaultP) of
        (0,    nt@[n,m,l], ds@[dl,dh,el,eh,fl,fh])
          | n >= 0 && m >= 0 && l >= 0  ->
            foldr
            (\(a,b) acc -> a:b:acc)
            []
            (zipWith3 updateRange nt [dl,el,fl] [dh,eh,fh])
        (step, (n:m:ns),   allds@(d:e:ds))
          | n <= m  -> n:m:(updateGrid (step+1) ns ds)
        (step, (n:ns),     allds@(d:e:ds))
          | n >= 0  ->
              let (n',m') = updateRange d e n
              in n':m':(updateGrid (step+1) ns ds)
          | otherwise             -> n:(n+e-d):(updateGrid (step+1) ns ds)
        (step, [],         allds) -> allds
        (step,  _,         _)     -> []
      updatedGridParameters :: [Int]
      updatedGridParameters = (updateGrid   0   gridPs   stdGridPs)
      updatedColors = let
        stdLevelMin = stdGridPs!!4
        stdLevelMax = stdGridPs!!5
        stdDefaultColor = stdColors!!(stdLevelMax-stdLevelMin+1)
        newLevelMin = updatedGridParameters!!4
        newLevelMax = updatedGridParameters!!5
        newLevelCard = newLevelMax - newLevelMin + 1
        in
          case colors of
            [] -> [ c | level <- [newLevelMin..newLevelMax]
                      , let c = if elem level [stdLevelMin..stdLevelMax]
                                then stdColors!!(level-stdLevelMin)
                                else stdDefaultColor] ++ [stdDefaultColor]
            c:cs  | length colors >  newLevelCard  ->
                      take (newLevelCard + 1) colors
                  | length (c:cs) == newLevelCard  ->
                      take newLevelCard colors ++ [stdDefaultColor]
                  | otherwise                      ->
                    sampleColorRange newLevelCard colors ++ [stdDefaultColor]
  in
    rawParamBuilder (updateCanvas 0 canvasPs stdCanvasPs)
                    updatedGridParameters
                    updatedColors


-- core

okbgen :: OkbParam -> Element
okbgen params = okbgen' doublePgc (opFloat2Double $ opDash params)


okbgen'
  :: forall a l.
     RealFloat a
  => PlaneGeoComputer a l
  ->  OkbParam' a
  -> Element
okbgen'
  pgc
  (OkbParam'
    (sizeX, sizeY)
    (scaleX, scaleY)
    (orX, orY)
    (octMin, octMax)
    (fifthsMin, fifthsMax)
    (thirdsMin, thirdsMax)
    colorMap) =
  svg sizeX sizeY scaleX scaleY orX orY keyboard where
  keyboard :: Element
  keyboard = mconcat (ingrid2 kbGrid makePoint)
  makePoint :: (Coordinates a, EulerGridCoord) -> Element
  makePoint ((posX, posY), EulerGridCoord octaves quints gterzes)
    = makeKeySvg
      relativeborderwidth
      (planarCoordKeyCorners gterzes)
      (colorMap gterzes)
      (posX, posY)
      (tonename octaves quints gterzes)
      (T.pack $
        "key:" ++ show octaves ++ ":" ++ show quints ++ ":" ++ show gterzes)
  -- argument: level (thirds)
  planarCoordKeyCorners :: Int -> [Coordinates a]
  planarCoordKeyCorners level = keyCorners pgc kbGrid level
  -- mind: size of cells: 1 !
  kbGrid :: EulerGrid a
  kbGrid = EulerGrid
    (1.0, 0.0)
    (quintx, quinty)
    (gterzx, gterzy)
    [octMin..octMax]
    [fifthsMin..fifthsMax]
    [thirdsMin..thirdsMax]
  quintx = (log 3.0 - log 2.0) / (log 2.0)
  gterzx = (log 5.0 - log 4.0) / (log 2.0)
  quinty = 0.2
  gterzy = -quinty* 3.5/7.0

-- drawing the keys
polyedges :: RealFloat a => Int -> a -> Int -> (a, a)
polyedges edges start_angle edge =
  ( (cos angle) * distance, (sin angle) * distance )
  where
    angle =
      ((fromIntegral edge) / (fromIntegral edges)* (fromIntegral 2) * pi)
      + start_angle
    distance = 0.5 / cos (pi / 6.0)

lineway :: RealFloat a => (a, a) -> (a, a) -> a -> (a, a)
lineway (x,y) (x',y') z = (x * (1.0-z) + x' * z, y * (1.0-z) + y' * z)

getedge :: RealFloat a => Int -> (a, a)
getedge = polyedges 6 (- pi / 6.0)

getedgeS :: RealFloat a => a -> (a, a)
getedgeS x = lineway (getedge f) (getedge $ f+1) (x - (fromIntegral f))
  where
    f = floor x

pointscalefactor :: RealFloat a => a
pointscalefactor = 0.06
