{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State.Strict (runState)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import MonadIEEE
  ( Binary8 (Binary8, unBinary8),
    IEEERepr (),
    Signed (S),
    Unsigned (U),
    assemble,
    explode,
    toFloat,
    toPositive,
  )
import Numeric.SpecFunctions (incompleteGamma)
import Numeric.Sum (kbn, sumVector)
import Statistics.Test.ChiSquared (chi2test)
import Statistics.Test.Types (Test (testSignificance))
import Statistics.Types (pValue)
import System.Random.SplitMix (mkSMGen)
import Test.Hspec
  ( Expectation,
    describe,
    hspec,
    pendingWith,
    shouldSatisfy,
    specify,
  )
import Uniform (uniformSigned)

main :: IO ()
main =
  let n = 10
   in hspec $ describe "uniform" $ do
        -- https://gitlab.com/christoph-conrads/rademacher-fpl/-/blob/bd1c5f0a778aef580cdb597506485ee622fcd0b5/test/uniform-real-distribution.cpp
        describe "rademacher-fpl tests" $ do
          -- specify "negative-interval" $ isUniform (S True 2 7) (U 0 1) n
          specify "negative-interval" $ isUniform (S True 0 1) (U 0 1) 1000
          specify "symmetric-interval" $ isUniform (S True 2 7) (U 3 0) n
          describe "same exponent" $ do
            specify "simple" $ isUniform (S False 7 1) (U 7 6) n
            specify "nextafter" $ isUniform (S False 2 4) (U 3 0) n
          describe "pow2" $ do
            specify "one-two" $ isUniform (S False 7 0) (U 8 0) (5 * n)
            specify "negative" $ pendingWith "both negative"
            describe "non-negative" $ do
              specify "basic" $ pendingWith "infinity"
              specify "vbin-overflow" $ pendingWith "infinity"
          describe "a-pow2" $ do
            specify "denormal-a" $ isUniform (S False 0 0) (U 1 7) n
            describe "normal-a" $ do
              specify "basic" $ isUniform (S False 1 0) (U 2 7) n
              specify "vbin-overflow" $ isUniform (S False 1 0) (U 8 7) (200 * n)
          describe "same-sign" $ do
            describe "normal-a" $ do
              specify "basic" $ isUniform (S False 1 3) (U 3 7) (10 * n)
              specify "vbin-overflow" $ isUniform (S False 1 3) (U 8 7) n
            specify "denormal-a" $ isUniform (S False 0 3) (U 2 7) (10 * n)
          describe "mixed-sign" $ do
            specify "basic" $ isUniform (S True 1 3) (U 2 1) n
            specify "denormals" $ isUniform (S True 0 7) (U 0 7) n
            describe "vbin-overflow" $ do
              specify "[a,b), abs(a) > abs(b)" $ isUniform (S True 4 5) (U 2 3) n
              specify "[a,b), abs(a) >> abs(b)" $ isUniform (S True 10 5) (U 2 3) n
        describe "own tests"
          $ specify "full range"
          $ isUniform (S False 0 0) (U 15 7) 1

-- newtype Result = { unResult :: V.Vector (Int, Double) }

-- instance Show Result where
--   show (Result v) = ...

isUniform ::
  Signed Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  Int ->
  Expectation
isUniform x b n = r' `shouldSatisfy` acceptH0
  where
    ex = V.map (* n) (expected x b) :: V.Vector Int
    ex' = V.map (fromIntegral :: Int -> Double) ex
    ob = observe (V.sum ex) x b
    r = V.izipWith (\i o e -> (toFloat (Binary8 (fromIntegral i)), o, e)) ob ex'
    r' = V.filter (\(_, o, e) -> o /= 0 || e /= 0) r
    acceptH0 :: V.Vector (Float, Int, Double) -> Bool
    acceptH0 i =
      -- pValue (testSignificance (fromJust (chi2test 0 i))) > 0.5
      chiSquaredP (V.map (\(_, o, e) -> (o, e)) i) > 0.5

expected ::
  Signed Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  V.Vector Int
expected x b = V.generate m (fromIntegral . weight x y . repr)
  where
    m = fromIntegral (maxBound :: Word8)
    repr = explode . Binary8 . fromIntegral
    y = toPositive b

observe ::
  Int ->
  Signed Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  V.Vector Int
observe n x b = V.create $ do
  let f = uniformSigned x b
      m = fromIntegral (maxBound :: Word8)
  v <- MV.new m
  let loop 0 _ = return ()
      loop m gen = do
        let (u, gen') = runState f gen
        MV.modify v succ (fromIntegral $ unBinary8 $ assemble u)
        loop (pred m) gen'
  loop n (mkSMGen 1337)
  return v

weight ::
  forall f e s.
  (IEEERepr f e s, Integral e) =>
  Signed f e s ->
  Signed f e s ->
  Signed f e s ->
  Word
weight x@(S _ ex _) y u@(S _ eu su)
  | x > u || u > y = 0
  | x == u = 1
  | y == u && su == 0 = w `div` 4
  | y == u = w `div` 2
  | su == 0 = 3 * (w `div` 4)
  | otherwise = w
  where
    w = (2 :: Word) ^ (succ eu - ex)

chiSquaredP ::
  V.Vector (Int, Double) ->
  Double
chiSquaredP v = 1 - cdf (sumVector kbn (V.map f v))
  where
    f :: (Int, Double) -> Double
    f (o, e) = sq (fromIntegral o - e) / e where sq x = x * x
    ---
    dof2 :: Double
    dof2 = fromIntegral (V.length v - 1) / 2
    ---
    cdf :: Double -> Double
    cdf x
      | x <= 0 = 0
      | otherwise = incompleteGamma dof2 (x / 2)
