{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Text.Printf (printf)
import Test.Hspec
import Test.QuickCheck
import Types
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
    it "interColor test 1" $ property $
      \cr cg cb (f :: Double) -> let RGB rr rg rb = interColor (RGB cr cg cb) (RGB cr cg cb) f in
                       and (zipWith (\x y -> (absDiff x y) < 1) [cr, cg, cb] [rr, rg, rb]) `shouldBe` True
    it "sampleColorRange" $ property $
      \(allCs :: [(Word8, Word8, Word8)]) -> let cs = map (\(r', g', b') -> RGB r' g' b') allCs in
                                 sampleColorRange (length cs) cs `shouldBe` cs

main :: IO ()
main = testColor
