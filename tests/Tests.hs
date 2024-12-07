module Main where

import Text.Printf (printf)
import Test.Hspec
import Test.QuickCheck
import Okbgen.Inner
import Data.Word (Word8)

absDiff :: Word8 -> Word8 -> Word8
absDiff x y
  | x <= y    = y - x
  | otherwise = x - y

testColor :: IO ()
testColor = hspec $ do
  describe "color related" $ do
    it "getColorCode and mkColorHTML" $ property $
      \r g b -> (orBlack . mkColorHTML . getColorCode) (RGB r g b) `shouldBe` (RGB r g b)
    it "interColor" $ property $
      \cr cg cb f -> let RGB rr rg rb = interColor (RGB cr cg cb) (RGB cr cg cb) f in
                       and (zipWith (\x y -> (absDiff x y) < 1) [cr, cg, cb] [rr, rg, rb]) `shouldBe` True
--    it "tolerantParamBuilder" $ property $
--      stdParams == tolerantParamBuilder stdParams (rawParamDeconstructor stdParams)
main :: IO ()
main = testColor
