{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Okbgen (Color(..), OkbParam(..), okbgen, Element) where

import Grid (Coordinates(..), ingrid2)
import EulerGrid (EulerGrid(..), EulerGridCoord(..), PlainCoord(..), keyCorners)

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


data Color = HTMLColor { getColorCode :: String }

labelColor = HTMLColor "#000000"
borderColor = HTMLColor "#000000"

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
                           opOriginOnCanvas :: (Float, Float),
                           opCanvasPxPerUnit :: (Float, Float),
                           opGridOctavesRange :: (Int, Int),
                           opGridFifthsRange :: (Int, Int),
                           opGridThirdsRange :: (Int, Int),
                           opKeyColors :: (Int -> Color) }

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



