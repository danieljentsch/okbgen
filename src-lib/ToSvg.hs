{-# LANGUAGE OverloadedStrings #-}

module ToSvg where

import Graphics.Svg ((<<-))


import qualified Data.Text as T
import qualified Graphics.Svg as Svg

import Types

labelColor = RGB 0 0 0
borderColor = RGB 0 0 0


makeKeySvg
  :: RealFloat a
  => a
  -> [(a, a)]
  -> Color
  -> (a, a)
  -> T.Text
  -> T.Text
  -> Svg.Element
makeKeySvg borderWidth corners fillColor (baseX, baseY) labelText idText =
  let
    key = Svg.path_
      [ Svg.Stroke_ <<- (T.pack $ getColorCode borderColor)
      , Svg.Stroke_width_ <<- (Svg.toText borderWidth)
      , Svg.Fill_ <<- (T.pack $ getColorCode fillColor)
      , Svg.D_ <<- case corners of
          []     -> mempty
          (c:cs) ->
            (uncurry Svg.mA) c
            <> foldMap (uncurry Svg.lA) cs
            <> Svg.z
      , Svg.Id_ <<- idText
      ]
    label = Svg.g_
      [Svg.Transform_ <<- (Svg.matrix 1.0 0.0 0.0 (-1.0) 0.0 0.0)]
      (Svg.text_
        [ Svg.Font_size_ <<- "0.04"
        , Svg.Text_anchor_ <<- "middle"
        , Svg.Fill_ <<- (T.pack $ getColorCode labelColor)]
        (Svg.toElement labelText))
  in
    Svg.g_ [Svg.Transform_ <<- (Svg.translate baseX baseY)] (key <> label)
  

svg
  :: RealFloat a
  => a
  -> a
  -> a
  -> a
  -> a
  -> a
  -> Svg.Element
  -> Svg.Element
svg width height scaleX scaleY orX orY content =
  Svg.doctype
  <> Svg.with
     (Svg.svg11_
       (Svg.g_
        [Svg.Transform_ <<- (Svg.matrix scaleX 0 0 (-scaleY) orX orY)]
        content))
     [ Svg.Version_ <<- "1.1"
     , Svg.Width_ <<- Svg.toText width
     , Svg.Height_ <<- Svg.toText height
     ]
