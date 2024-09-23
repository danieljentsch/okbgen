{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

module Okbgen (bs_keyboard, text_keyboard, result) where

import Graphics.Svg
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Ix as DI
import qualified LinesGeometry as LG

import Grid
import EulerGrid


-- data Layout = LayoutNode

-- data LayoutNode = [];

cellsize, celltowidth, relativeborderwidth :: Double
cellsize = 30.0
celltowidth = 0.1
relativeborderwidth = 0.001

height, width, originx, originy, scalex, scaley :: Int
height = 1080
width = 1920
originx = 1000
originy = 540
scalex = 400
scaley = -400
{--
height, width, originx, originy :: Int
width = round $ cellsize * (keyboardwidth) / celltowidth
  where
    (keyboardwidth, keyboardheight) = (cx sizecoord, cy sizecoord)
    sizecoord = snd fr -# fst fr
    fr = frame kbGrid makePointFrame

height = (round $ cellsize * (keyboardheight) / celltowidth)
  where
    (keyboardwidth, keyboardheight) = (cx sizecoord, cy sizecoord)
    sizecoord = snd fr -# fst fr
    fr = frame kbGrid makePointFrame

originx = round $ cellsize * (offsetx) / celltowidth
  where
    (keyboardwidth, keyboardheight) = asTuple sizecoord
    (offsetx, offsety) = asTuple offset
    offset = zero -# fst fr
    sizecoord = snd fr -# fst fr
    fr = frame kbGrid makePointFrame

originy = (round $ cellsize * (offsety) / celltowidth)
  where
    (keyboardwidth, keyboardheight) = asTuple sizecoord
    (offsetx, offsety) = asTuple offset
    offset = zero -# fst fr
    sizecoord = snd fr -# fst fr
    fr = frame kbGrid makePointFrame
--}

-- labels for keys.
tonename :: Int -> Int -> Int -> T.Text
tonename octaves quints gterzes = commata <> oktavetonename
  where
    commata                     | gterzes > 0          = stimesMonoid gterzes ","
                                | gterzes == 0         = ""
                                | gterzes < 0          = stimesMonoid (-gterzes) "\'"
    oktavetonename              | whichoctave > 0      = tonenameroot 0 <> stimesMonoid whichoctave ("\'" :: T.Text)
                                | whichoctave == 0     = tonenameroot 0
                                | whichoctave < 0      = tonenameroot 1 <> stimesMonoid (-1 - whichoctave) ","
    whichoctave                                        = (floor logdesttobase) :: Int
    tonenameroot cas                                   = makecase cas $ quintrow (quints + gterzes * 4)
    logdesttobase                                      = (log 2.0 * fromIntegral octaves + log (3.0/2.0) * fromIntegral quints + log (5.0/4.0) * fromIntegral gterzes) / log 2.0
    makecase cas txt            | cas == 0             = (txt :: T.Text)
                                | cas == 1             = T.toTitle (txt :: T.Text) --T.toUpper (T.take 1 (txt :: T.Text)) <> T.takeEnd 1 (txt :: T.Text)
    quintrow n                  |         n < (-8)     = qrflat!!(mod (n+1) 7) <> stimesMonoid ((-1) - div (n+1) 7 ) ("es" :: T.Text)
                                | DI.inRange (-8, -2) n = qrflat!!((n+1)+7)
                                | DI.inRange (-1, 5) n  = qrbase!!(n+1)
                                | DI.inRange (6, 12) n = qrsharp!!((n+1)-7)
                                | 13   <= n            = qrsharp!!(mod (n+1) 7) <> stimesMonoid (div (n+1) 7 - 1) ("is" :: T.Text)
    qrbase                                             = ["f","c","g","d","a","e","h"] :: [T.Text]
    qrsharp                                            = ["fis","cis","gis","dis","ais","eis","his"] :: [T.Text]
    qrflat                                             = ["fes","ces","ges","des","as","es","b"] :: [T.Text]


-- svg boilerplate
svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- (toText (fromIntegral width)), Height_ <<- (toText (fromIntegral height))]


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

data Color = Black | White | Gray | DarkGray
           | Brown | DarkBrown | Ebony
           | BlueBrown | BlueDarkBrown
           | Yellow | DarkYellow
           | Blue | DarkBlue | LabelColor | BorderColor

getCCode :: Color -> T.Text
getCCode Black = "#000000"
getCCode White = "#ffffff"
getCCode Gray = "#aaaaaa"
getCCode DarkGray = "#888888"
getCCode Ebony = "#ffeecc"
getCCode Brown = "#e7aa66"
getCCode DarkBrown = "#554411"
getCCode BlueBrown = "#bbaa99"
getCCode BlueDarkBrown = "#444444"
getCCode Yellow = "#777700"
getCCode DarkYellow = "#333300"
getCCode Blue = "#0000ff"
getCCode DarkBlue = "#000077"
getCCode LabelColor = "#000000"
getCCode BorderColor = "#000000"


fotoColors :: Int -> T.Text
fotoColors (-2) = "#0f0e14"
fotoColors (-1) = "#9db2c5"
fotoColors    0 = "#c2c0b9"
fotoColors    1 = "#c48569"
fotoColors    2 = "#3f2a1e"

edgepathD :: [Int] -> T.Text
edgepathD (c:cs) = uncurry mA (getedge c)
                  <> mconcat (map (uncurry lA . getedge) (c:cs))
                  <> z
                  <> uncurry mA (getedge c)

edgepathDS :: RealFloat a => [a] -> T.Text
edgepathDS (c:cs) = uncurry mA (getedgeS c)
                  <> mconcat (map (uncurry lA . getedgeS) (c:cs))
                  <> z
                  <> uncurry mA (getedgeS c)

celltile :: RealFloat a => [a] -> Color -> Element
celltile edgep c = path_ [Stroke_ <<- getCCode Gray,
                        Stroke_width_ <<- (toText (relativeborderwidth)),
                        Fill_ <<- getCCode c,
                        D_ <<- edgepathDS edgep]



cell :: Element
cell = celltile [0.0, 1.0, 2.0, 3.0, 4.0, 5.0] Ebony
       <> celltile [-2.0/3.0, 1.0+1.0/3.0, 1.0+2.0/3.0, -1.0] DarkYellow
       <> celltile [2.0, 2.0+1.0/3.0, 4.0+1.0/3.0, 4.0+2.0/3.0] DarkBlue
       <> celltile [0.0,  1.0, -2.0/3.0] Blue
       <> celltile [2.0+1.0/3.0, 3.0, 4.0] Yellow

levelColor :: Int -> T.Text
levelColor (-2) = getCCode BlueDarkBrown
levelColor (-1) = getCCode BlueBrown
levelColor 0 = getCCode Ebony
levelColor 1 = getCCode Brown
levelColor 2 = getCCode DarkBrown
levelColor _ = getCCode Black


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


pointpathD :: Int -> T.Text
pointpathD level = (\(c:cs) -> ((uncurry mA) c <> mconcat (map (uncurry lA) cs) <> (uncurry mA) c <> z))
                   $ map coordfromTuple (raute $ ((pointFrame level) :: (PlainCoord, PlainCoord)))

-- argument: level (thirds)
plainCoordKeyCorners :: Int -> [PlainCoord]
plainCoordKeyCorners level = [ PlainCoord x y | (x, y) <- (map LG.toPair $ keyCorners kbGrid level) ]

-- argument: level (thirds)
customPointPath :: Int -> T.Text
customPointPath level = (\(c:cs) -> ((uncurry mA) c <> mconcat (map (uncurry lA) cs) <> (uncurry mA) c <> z))
                   $ map coordfromTuple (plainCoordKeyCorners level)

{-
point :: Int -> Element
point level = path_ [Stroke_ <<- getCCode Gray, Stroke_width_ <<- (toText relativeborderwidth), Fill_ <<- levelColor level, D_ <<- pointpathD level]
-}

point :: Int -> Int -> Int -> Element
point oktave quinte level = path_ [Stroke_ <<- getCCode BorderColor, Stroke_width_ <<- (toText relativeborderwidth), Fill_ <<- fotoColors level, D_ <<- customPointPath level,
                     Id_ <<- (T.pack $ "key:" ++ show oktave ++ ":" ++ show quinte ++ ":" ++ show level)]

moveElement :: RealFloat a => Element -> a -> a -> Element
moveElement e x y = g_ [Transform_ <<- (translate x y)] e

makeCell :: Coordinates c a => (c, Bool) -> Element
makeCell (pos, _) = g_ [Transform_ <<- (translate (cx pos) (cy pos))] cell


makePoint :: Coordinates c a => (c, EulerGridCoord) -> Element
makePoint (pos, EulerGridCoord octaves quints gterzes) = g_ [Transform_ <<- (translate (cx pos) (cy pos))]
                                              (point octaves quints gterzes <>
                                               (g_ [Transform_ <<- (matrix 1.0 0.0 0.0 (-1.0) 0.0 0.0)]
                                                 (text_   [ X_ <<- "0", Y_ <<- "0", Font_size_ <<- "0.04", Text_anchor_ <<- "middle", Fill_ <<- (getCCode LabelColor)]
                                                   (toElement (tonename octaves quints gterzes)))))


-- compute frame
makePointFrame :: Coordinates c a => (c, EulerGridCoord) -> (c, c)
makePointFrame (pos, EulerGridCoord octaves quints gterzes) = tupleMap (pos +#) $ pointFrame gterzes

-- mind: size of cells: 1 !
kbGrid :: EulerGrid
kbGrid = EulerGrid (PlainCoord 1.0 0.0) (PlainCoord quintx quinty) (PlainCoord gterzx gterzy) [(-8)..8] [(-10)..10] [(-2)..2]
  where
    quintx = (log 3.0 - log 2.0) / (log 2.0)
    gterzx = (log 5.0 - log 4.0) / (log 2.0)
    quinty = 0.2
    gterzy = -quinty* 3.5/7.0

keyboard :: Element
keyboard = mconcat (ingrid2 kbGrid makePoint)
--keyboard = g_ [] $ (ingrid Stdhexgrid cell cellsx cellsy)

picture = g_ [Transform_ <<- (matrix sx 0 0 sy (ox) (oy))] keyboard
  where
    w = fromIntegral width
    h = fromIntegral height
    ox = fromIntegral originx
    oy = fromIntegral originy
    sx = fromIntegral scalex
    sy = fromIntegral scaley

result :: Element
result = svg picture

bs_keyboard = renderBS result
text_keyboard = renderText result
