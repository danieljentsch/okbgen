module Main where

import Okbgen
fotoColors :: Int -> String
fotoColors (-2) = "#0f0e14"
fotoColors (-1) = "#9db2c5"
fotoColors    0 = "#c2c0b9"
fotoColors    1 = "#c48569"
fotoColors    2 = "#3f2a1e"
fotoColors    _ = "#ffffff"

height, width, originx, originy, scalex, scaley :: Float
height = 1080
width = 1920
originx = 1000
originy = 540
scalex = 400
scaley = -400

octsMin, octsMax, fifthsMin, fifthsMax, thirdsMin, thirdsMax :: Int
octsMin = -8
octsMax = 8
fifthsMin = -10
fifthsMax = 10
thirdsMin = -2
thirdsMax = 2

stdParams :: OkbParam
stdParams =  OkbParam { opCanvasSize = (width, height),
                        opOriginOnCanvas = (originx, originy),
                        opCanvasPxPerUnit = (scalex, scaley),
                        opGridOctavesRange = (octsMin, octsMax),
                        opGridFifthsRange = (fifthsMin, fifthsMax),
                        opGridThirdsRange = (thirdsMin, thirdsMax),
                        opKeyColors = HTMLColor . fotoColors }
main :: IO ()
main = print $ okbgen stdParams
main :: IO ()
