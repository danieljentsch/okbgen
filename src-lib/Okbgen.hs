{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Okbgen (Color(..), mkColorHTML, getColorCode, stdParams, orBlack, OkbParam(..), tolerantParamBuilder, okbgen, Element) where

import Grid (Coordinates(..), ingrid2)
import EulerGrid (EulerGrid(..), EulerGridCoord(..), PlainCoord(..), keyCorners)
import Data.Word (Word8)
import Data.Char (digitToInt)
import Numeric (showHex)
import Text.Printf (printf)

import Graphics.Svg (Element,
                     doctype,
                     lA,
                     mA,
                     matrix,
                     toElement,
                     toText,
                     translate,
                     renderText,
                     renderBS,
                     with,
                     z,
                     AttrTag(Transform_, Fill_, Font_size_, Text_anchor_, X_, Y_, D_, Id_, Stroke_width_, Stroke_, Height_, Width_, Version_),
                     text_,
                     g_,
                     path_,
                     svg11_,
                     (<<-))
import Data.Semigroup (stimesMonoid)
import Data.Text (Text, pack)
import qualified Data.Text as T (toTitle, pack)
import qualified Data.Ix as DI (inRange)
import qualified PlainGeometry as PG (toPair)

data Color = RGB Word8 Word8 Word8 deriving (Show,Eq)


black :: Color
black = RGB 0 0 0

orBlack :: Maybe Color -> Color
orBlack (Just color) = color
orBlack Nothing = black

getColorCode :: Color -> String
getColorCode (RGB r g b) = printf ("#" ++ concat (replicate 3 "%02x")) r g b

mkColorHTML :: String -> Maybe Color
mkColorHTML str = case str of
  '#':rst -> let len = length rst in
               if len == length (filter (flip (elem @[]) "0123456789abcdef") rst)
               then
                 case len of
                   6 -> Just $ let ([r,g,b],_,_) =
                                     foldr (\hexDigit (accl, digitIndex, lastDigit) -> (case digitIndex of
                                                                                          0 -> (accl, 1, (fromIntegral $ digitToInt hexDigit))
                                                                                          1 -> ((lastDigit+16*(fromIntegral $ digitToInt hexDigit)):accl, 0, 0)))
                                           ([], 0, 0)
                                           rst
                               in
                                 RGB r g b
                   3 -> Just $ let [r,g,b] = foldr (\hexDigit accl -> ((16+1)*(fromIntegral $ digitToInt hexDigit)):accl) [] rst in (RGB r g b)
                   _ -> Nothing
                 else Nothing
  _       -> Nothing

interColor :: Color -> Color -> Float -> Color
interColor c@(RGB cr cg cb) d@(RGB dr dg db) f
    | f <= 0    = c
    | f >= 1    = d
    | otherwise = RGB (wm cr dr) (wm cg dg) (wm cb db)
  where
    wm a b = round $ (1-f) * (fromIntegral a) + f * (fromIntegral b)

labelColor = RGB 0 0 0
borderColor = RGB 0 0 0

-- data Layout = LayoutNode

-- data LayoutNode = [];

cellsize, celltowidth, relativeborderwidth :: Double
cellsize = 30.0
celltowidth = 0.1
relativeborderwidth = 0.001


-- labels for keys.
tonename :: Int -> Int -> Int -> Text
tonename octaves quints gterzes = commata <> oktavetonename
  where
    commata                     | gterzes > 0          = stimesMonoid gterzes ","
                                | gterzes == 0         = ""
                                | gterzes < 0          = stimesMonoid (-gterzes) "\'"
    oktavetonename              | whichoctave > 0      = tonenameroot 0 <> stimesMonoid whichoctave ("\'" :: Text)
                                | whichoctave == 0     = tonenameroot 0
                                | whichoctave < 0      = tonenameroot 1 <> stimesMonoid (-1 - whichoctave) ","
    whichoctave                                        = (floor logdesttobase) :: Int
    tonenameroot cas                                   = makecase cas $ quintrow (quints + gterzes * 4)
    logdesttobase                                      = (log 2.0 * fromIntegral octaves + log (3.0/2.0) * fromIntegral quints + log (5.0/4.0) * fromIntegral gterzes) / log 2.0
    makecase cas txt            | cas == 0             = (txt :: Text)
                                | cas == 1             = T.toTitle (txt :: Text) --T.toUpper (T.take 1 (txt :: Text)) <> T.takeEnd 1 (txt :: Text)
    quintrow n                  |         n < (-8)     = qrflat!!(mod (n+1) 7) <> stimesMonoid ((-1) - div (n+1) 7 ) ("es" :: Text)
                                | DI.inRange (-8, -2) n = qrflat!!((n+1)+7)
                                | DI.inRange (-1, 5) n  = qrbase!!(n+1)
                                | DI.inRange (6, 12) n = qrsharp!!((n+1)-7)
                                | 13   <= n            = qrsharp!!(mod (n+1) 7) <> stimesMonoid (div (n+1) 7 - 1) ("is" :: Text)
    qrbase                                             = ["f","c","g","d","a","e","h"] :: [Text]
    qrsharp                                            = ["fis","cis","gis","dis","ais","eis","his"] :: [Text]
    qrflat                                             = ["fes","ces","ges","des","as","es","b"] :: [Text]

data OkbParam = OkbParam { opCanvasSize :: (Float, Float),
                           opCanvasPxPerUnit :: (Float, Float),
                           opOriginOnCanvas :: (Float, Float),
                           opGridOctavesRange :: (Int, Int),
                           opGridFifthsRange :: (Int, Int),
                           opGridThirdsRange :: (Int, Int),
                           opKeyColors :: (Int -> Color) }

-- standard values
stdColors :: Int -> String
stdColors (-2) = "#0f0e14"
stdColors (-1) = "#9db2c5"
stdColors    0 = "#c2c0b9"
stdColors    1 = "#c48569"
stdColors    2 = "#3f2a1e"
stdColors    _ = "#ffffff"

stdHeight, stdWidth, stdScalex, stdScaley, stdOriginx, stdOriginy :: Float
stdHeight = 1080
stdWidth = 1920
stdScalex = 400
stdScaley = -400
stdOriginx = 1000
stdOriginy = 540

stdOctsMin, stdOctsMax, stdFifthsMin, stdFifthsMax, stdThirdsMin, stdThirdsMax :: Int
stdOctsMin = -8
stdOctsMax = 8
stdFifthsMin = -10
stdFifthsMax = 10
stdThirdsMin = -2
stdThirdsMax = 2

stdParams :: OkbParam
stdParams =  OkbParam { opCanvasSize = (stdWidth, stdHeight),
                        opCanvasPxPerUnit = (stdScalex, stdScaley),
                        opOriginOnCanvas = (stdOriginx, stdOriginy),
                        opGridOctavesRange = (stdOctsMin, stdOctsMax),
                        opGridFifthsRange = (stdFifthsMin, stdFifthsMax),
                        opGridThirdsRange = (stdThirdsMin, stdThirdsMax),
                        opKeyColors = orBlack . mkColorHTML . stdColors }


-- may throw an error!  check arguments
rawParamBuilder :: [Float] -> [Int] -> [Color] -> OkbParam
rawParamBuilder [c1,c2,c3,c4,c5,c6] [g1,g2,g3,g4,g5,g6] colors =
  OkbParam (c1, c2) (c3, c4) (c5, c6) (g1, g2) (g3, g4) (g5, g6) colFun
  where
    colFun x | x >= g5 && x <= g6 = colors!!(x - g5)
             | otherwise          = colors!!(g6 - g5 + 1)

rawParamDeconstructor :: OkbParam -> ([Float],[Int],[Color])
rawParamDeconstructor (OkbParam (c1, c2) (c3, c4) (c5, c6) (g1, g2) (g3, g4) (g5, g6) colFun) =
  ([c1,c2,c3,c4,c5,c6], [g1,g2,g3,g4,g5,g6], colors)
  where
    colors = map colFun [g5..(g6+1)]

--TODO: Check if (a == 0) false implies that (x / a) is not an error (for floats).
updateHeightKeepRatio oldWidth oldHeight newWidth
  | oldWidth == 0  = oldHeight
  | otherwise      = oldHeight * newWidth / oldWidth

-- try to center an integer range around center of given range; if necessary, move slightly up
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
                (_, _)              -> [rangePoints!!0]
                                         ++ [ interColor (rangePoints!!rpIndex) (rangePoints!!(rpIndex+1)) fraction
                                              | i <- [1..(n-2)],
                                                let (rpIndex, fraction) = properFraction (fromIntegral (length rangePoints) * fromIntegral i / (fromIntegral (n-1) :: Float) )]
                                         ++ [rangePoints!!(n-1)]

tolerantParamBuilder :: OkbParam -> [Float] -> [Int] -> [Color] -> OkbParam
tolerantParamBuilder defaultP canvasPs gridPs colors =
  let (stdCanvasPs, stdGridPs, stdColors) = rawParamDeconstructor defaultP
      -- update[what] step <remaining new parameters> <remaining std parameters> -> <resulting parameters>
      -- [what] = Canvas
      -- steps: (0 canvas size) (1 (canvas pixel / unit of image)) (2 position of image origin on canvas)
      updateCanvas step (n:m:ns) (d:e:ds) = (if step < 3 then  n:m:(updateCanvas (step+1) ns ds) else [])
      updateCanvas 0    [n]      (d:e:ds) = n:(updateHeightKeepRatio d e n):ds -- updating canvas size
      updateCanvas 1    [n]      (d:e:ds) = n:(updateHeightKeepRatio d e n):ds -- updating (canvas pixel / unit of image)
      updateCanvas 2    [n]      (d:e:ds) = [n,e] -- updating position of image origin on canvas
      updateCanvas step [] allds@(d:e:ds) = allds
      updateCanvas _    _              ds = ds -- should never occur
      -- [what] = Grid
      -- steps: (0 octave range) (1 fifths range) (2 position of image origin on canvas)
      updateGrid :: Int -> [Int] -> [Int] -> [Int]
      updateGrid step newP defaultP = case (step, newP, defaultP) of
        (0,    nt@[n,m,l], ds@[dl,dh,el,eh,fl,fh])   | n >= 0 && m >= 0 && l >= 0  -> foldr (\(a,b) acc -> a:b:acc) [] ({-new ranges as pairs-} zipWith3 updateRange nt [dl,el,fl] [dh,eh,fh])
        (step, (n:m:ns),   allds@(d:e:ds))           | n <= m                      -> n:m:(updateGrid (step+1) ns ds)
        (step, (n:ns),     allds@(d:e:ds))           | n >= 0                      -> let (n',m') = updateRange d e n in n':m':(updateGrid (step+1) ns ds)
                                                     | otherwise                   -> n:(n+e-d):(updateGrid (step+1) ns ds)
        (step, [],         allds)                                                  -> allds
        (step,  _,         _)                                                      -> []
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
            []                                     -> [ c | level <- [newLevelMin..newLevelMax], let c = if elem level [stdLevelMin..stdLevelMax]
                                                                                                         then stdColors!!(level-stdLevelMin)
                                                                                                         else stdDefaultColor] ++ [stdDefaultColor]
            c:cs  | length colors >  newLevelCard  -> take (newLevelCard + 1) colors
                  | length (c:cs) == newLevelCard  -> take newLevelCard colors ++ [stdDefaultColor]
                  | otherwise                      -> sampleColorRange newLevelCard colors ++ [stdDefaultColor]
  in
    rawParamBuilder (updateCanvas 0 canvasPs stdCanvasPs)
                    updatedGridParameters
                    updatedColors


okbgen :: OkbParam -> Element
okbgen (OkbParam (sizeX, sizeY) (orX, orY) (scaleX, scaleY) (octMin, octMax) (fifthsMin, fifthsMax) (thirdsMin, thirdsMax) colorMap) = svg picture where
  picture = g_ [Transform_ <<- (matrix scaleX 0 0 scaleY (orX) (orY))] keyboard
  -- svg boilerplate
  svg :: Element -> Element
  svg content = doctype <> with (svg11_ content) [Version_ <<- "1.1",
                                                  Width_ <<- (pack $ show sizeX),
                                                  Height_ <<- (pack $ show sizeY)]
  keyboard :: Element
  keyboard = mconcat (ingrid2 kbGrid makePoint)
  makePoint :: Coordinates c a => (c, EulerGridCoord) -> Element
  makePoint (pos, EulerGridCoord octaves quints gterzes) = g_ [Transform_ <<- (translate (cx pos) (cy pos))]
                                                           (point octaves quints gterzes <>
                                                            (g_ [Transform_ <<- (matrix 1.0 0.0 0.0 (-1.0) 0.0 0.0)]
                                                             (text_   [ X_ <<- "0", Y_ <<- "0", Font_size_ <<- "0.04", Text_anchor_ <<- "middle", Fill_ <<- (pack $ getColorCode labelColor)]
                                                              (toElement (tonename octaves quints gterzes)))))
  point :: Int -> Int -> Int -> Element
  point oktave quinte level = path_ [Stroke_ <<- (pack $ getColorCode borderColor), Stroke_width_ <<- (toText relativeborderwidth), Fill_ <<- (pack $ getColorCode (colorMap level)), D_ <<- customPointPath level,
                                     Id_ <<- (T.pack $ "key:" ++ show oktave ++ ":" ++ show quinte ++ ":" ++ show level)]
  -- argument: level (thirds)
  plainCoordKeyCorners :: Int -> [PlainCoord]
  plainCoordKeyCorners level = [ PlainCoord x y | (x, y) <- (map PG.toPair $ keyCorners kbGrid level) ]
  -- argument: level (thirds)
  customPointPath :: Int -> Text
  customPointPath level = (\(c:cs) -> ((uncurry mA) c <> mconcat (map (uncurry lA) cs) <> (uncurry mA) c <> z))
                          $ map coordfromTuple (plainCoordKeyCorners level)
  -- mind: size of cells: 1 !
  kbGrid :: EulerGrid
  kbGrid = EulerGrid (PlainCoord 1.0 0.0) (PlainCoord quintx quinty) (PlainCoord gterzx gterzy) [octMin..octMax] [fifthsMin..fifthsMax] [thirdsMin..thirdsMax]
  quintx = (log 3.0 - log 2.0) / (log 2.0)
  gterzx = (log 5.0 - log 4.0) / (log 2.0)
  quinty = 0.2
  gterzy = -quinty* 3.5/7.0

-- drawing the keys
polyedges :: RealFloat a => Int -> a -> Int -> (a, a)
polyedges edges start_angle edge = ( (cos angle) * distance
                                   , (sin angle) * distance )
  where
    angle = ((fromIntegral edge) / (fromIntegral edges) * (fromIntegral 2) * pi) + start_angle
    distance = 0.5 / cos (pi / 6.0)

lineway :: RealFloat a => (a, a) -> (a, a) -> a -> (a, a)
lineway (x,y) (x',y') z = (x * (1.0-z) + x' * z, y * (1.0-z) + y' * z)

getedge :: RealFloat a => Int -> (a, a)
getedge = polyedges 6 (- pi / 6.0)

getedgeS :: RealFloat a => a -> (a, a)
getedgeS x = lineway (getedge f) (getedge $ f+1) (x - (fromIntegral f))
  where
    f = floor x

edgepathD :: [Int] -> Text
edgepathD (c:cs) = uncurry mA (getedge c)
                  <> mconcat (map (uncurry lA . getedge) (c:cs))
                  <> z
                  <> uncurry mA (getedge c)

edgepathDS :: RealFloat a => [a] -> Text
edgepathDS (c:cs) = uncurry mA (getedgeS c)
                  <> mconcat (map (uncurry lA . getedgeS) (c:cs))
                  <> z
                  <> uncurry mA (getedgeS c)

pointscalefactor :: RealFloat a => a
pointscalefactor = 0.06

-- helper
tupleMap :: (a -> c) -> (a,a) -> (c,c)
tupleMap f (x1, x2) = (f x1, f x2)

pointFrame :: (RealFloat a, Coordinates c a) => Int -> (c,c)
pointFrame   0  = tupleMap ((pointscalefactor •) . coord) ((-1.0,-8.0),(1.0,8.0))
pointFrame   1  = tupleMap ((pointscalefactor •) . coord) ((-1.0,-6.0),(1.0,6.0))
pointFrame (-1) = tupleMap ((pointscalefactor •) . coord) ((-1.0,-6.0),(1.0,6.0))
pointFrame   2  = tupleMap ((pointscalefactor •) . coord) ((-1.0,-2.0),(1.0,2.0))
pointFrame (-2) = tupleMap ((pointscalefactor •) . coord) ((-1.0,-2.0),(1.0,2.0))

raute :: Coordinates c a => (c,c) -> [c]
raute (ll,ru) = map coord [(cx ru,0), (0, cy ru), (cx ll, 0), (0, cy ll)]


pointpathD :: Int -> Text
pointpathD level = (\(c:cs) -> ((uncurry mA) c <> mconcat (map (uncurry lA) cs) <> (uncurry mA) c <> z))
                   $ map coordfromTuple (raute $ ((pointFrame level) :: (PlainCoord, PlainCoord)))


moveElement :: RealFloat a => Element -> a -> a -> Element
moveElement e x y = g_ [Transform_ <<- (translate x y)] e




-- compute frame
makePointFrame :: Coordinates c a => (c, EulerGridCoord) -> (c, c)
makePointFrame (pos, EulerGridCoord octaves quints gterzes) = tupleMap (pos +#) $ pointFrame gterzes


--keyboard = g_ [] $ (ingrid Stdhexgrid cell cellsx cellsy)



