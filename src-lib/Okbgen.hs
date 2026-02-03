module Okbgen
  ( Color(..)
  , mkColorHTML
  , getColorCode
  , tolerantParamBuilder
  , stdParams
  , rawParamDeconstructor
  , orBlack
  , OkbParam(..)
  , okbgen
  , Element
  ) where

import Graphics.Svg (Element)

import Okbgen.Inner
import Types
