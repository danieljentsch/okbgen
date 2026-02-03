{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Exception (try, Exception, SomeException, evaluate)
import Control.Monad (join)
import Data.Either.Combinators (mapLeft)
import Data.Functor.Compose (Compose(..))
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import Okbgen


class SwapMonads m1 m2 where
  mSwap :: m1 (m2 a) -> m2 (m1 a)

instance (Monad m1, Monad m2, SwapMonads m1 m2) => Monad (Compose m2 m1) where
  return = pure
  (>>=) p t = Compose (join <$> join (mSwap <$> getCompose (getCompose <$> (t <$> p))))

instance SwapMonads (Either a) IO where
  mSwap = \case
    Left a -> return (Left a)
    Right io -> Right <$> io

instance Exception e => MonadFail (Compose IO (Either (Maybe e, String))) where
  fail errmsg = Compose . return . Left . (\str -> (Nothing, str)) $ errmsg

tryReadMultiple :: (Read b) => String -> [String] -> Compose IO (Either (Maybe SomeException, String)) [b]
tryReadMultiple failmsg = traverse (Compose . ((mapLeft (\e -> (Just e, failmsg))) <$>) . try . evaluate . read)


okbgenI026iA :: [String] -> IO (Either String String)
okbgenI026iA = fmap (mapLeft snd) . getCompose . ( fmap (show . okbgen) . (\case
    []            -> pure stdParams
    twoPar@[_, _] -> do
      [width', height'] <- tryReadMultiple ("Could not parse floating point width or height argument") twoPar
      pure stdParams { opCanvasSize = (width', height'),
                       opOriginOnCanvas = (width' / 2, height' / 2)}
    sixPar@[_, _, _, _, _, _] -> do
      [width', height', originx', originy', scalex', scaley'] <- tryReadMultiple ("Could not parse floating point width, height, origin (x or y), or scaling (x or y) argument") sixPar
      pure stdParams { opCanvasSize = (width', height'),
                       opOriginOnCanvas = (originx', originy'),
                       opCanvasPxPerUnit = (scalex', scaley')}
    eighteenPar@[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] -> do
      [width', height', originx', originy', scalex', scaley'] <- tryReadMultiple ("Could not parse floating point width, height, origin (x or y), or scaling (x or y) argument") (take 6 eighteenPar)
      [octsMin', octsMax', fifthsMin', fifthsMax', thirdsMin', thirdsMax'] <- tryReadMultiple ("Could not parse integer octaves, fifths or thirds min or max argument") (take 6 $ drop 6 eighteenPar)
      let [colLL, colL, col, colH, colHH, colDefault] = (map (orBlack . mkColorHTML) $ take 6 $ drop 12 eighteenPar)
      pure stdParams { opCanvasSize = (width', height'),
                       opOriginOnCanvas = (originx', originy'),
                       opCanvasPxPerUnit = (scalex', scaley'),
                       opGridOctavesRange = (octsMin', octsMax'),
                       opGridFifthsRange = (fifthsMin', fifthsMax'),
                       opGridThirdsRange = (thirdsMin', thirdsMax'),
                       opKeyColors =
                         (\level -> case level of
                                      (-2) -> colLL
                                      (-1) -> colL
                                      ( 0) -> col
                                      ( 1) -> colH
                                      ( 2) -> colHH
                                      _    -> colDefault)}
    args -> Compose . return . Left $ (Nothing, "Wrong amount of arguments:  You gave " ++ show (length args) ++ ", but any of 0, 2, 6 or 18 are required.")))

main :: IO ()
main = do
  args <- getArgs
  genRes <- okbgenI026iA args
  case genRes of
    Left errmsg -> hPutStrLn stderr $
      errmsg ++ "\n"
      ++ "Usage:\n \
         \ <command> [<canvas-width> <canvas-height>\n\
         \             [<origin-on-canvas-x> <origin-on-canvas-y>\n\
         \               <scale-factor-x> <sace-factor-y>\n\
         \               [<octave-level-low> <octave-level-high>\n\
         \                 <fifths-level-low> <fifths-level-hign>\n\
         \                 <major-thirds-level-low> <major-thirds-level-high>\n\
         \                 <HTML-color-thirds-level-(-2)> ...\n\
         \                                        <HTML-color-thirds-level-2>\n\
         \                 <HTML-color-thirds-level-default>]]]"
    Right keyboard -> putStrLn keyboard
