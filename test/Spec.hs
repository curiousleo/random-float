{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State.Strict (runState)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)
import MonadIEEE
  ( Binary8,
    IEEERepr (maxSignificand),
    Unsigned (U),
    toPositive,
    toUnsigned,
  )
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
  let p = 0.5
      n = 10
   in hspec $ describe "uniform" $ do
        describe "rademacher tests" $ do
          specify "negative-interval" $ pendingWith "negative"
          specify "symmetric-interval" $ pendingWith "negative"
          describe "same exponent" $ do
            specify "simple" $ isUniform (U 7 1) (U 7 6) n p
            specify "nextafter" $ isUniform (U 2 4) (U 3 0) n p
          describe "pow2" $ do
            specify "one-two" $ isUniform (U 7 0) (U 8 0) (5 * n) p
            specify "negative" $ pendingWith "negative"
            describe "non-negative" $ do
              specify "basic" $ pendingWith "infinity"
              specify "vbin-overflow" $ pendingWith "infinity"
          describe "a-pow2" $ do
            specify "denormal-a" $ isUniform (U 0 0) (U 1 7) n p
            describe "normal-a" $ do
              specify "basic" $ isUniform (U 1 0) (U 2 7) n p
              specify "vbin-overflow" $ isUniform (U 1 0) (U 8 7) n 0.2
          describe "same-sign" $ do
            describe "normal-a" $ do
              specify "basic" $ isUniform (U 1 3) (U 3 7) (10 * n) p
              specify "vbin-overflow" $ isUniform (U 1 3) (U 8 7) n p
            specify "denormal-a" $ isUniform (U 0 3) (U 2 7) (10 * n) p
          describe "mixed-sign" $ do
            specify "basic" $ pendingWith "negative"
            specify "denormals" $ pendingWith "negative"
            describe "vbin-overflow" $ do
              specify "[a,b), abs(a) > abs(b)" $ pendingWith "negative"
              specify "[a,b), abs(a) >> abs(b)" $ pendingWith "negative"
        describe "own tests" $ do
          specify "full range" $
            isUniform (U 0 0) (U 15 7) 1 p

isUniform ::
  Unsigned Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  Int ->
  Double ->
  Expectation
isUniform x y n pExpected =
  let ept = V.map (* n) (expected x y) :: V.Vector Int
      obs = observed (V.sum ept) x y
      inp = V.zip obs (V.map (fromIntegral :: Int -> Double) ept)
      acceptNullHypothesis :: V.Vector (Int, Double) -> Bool
      acceptNullHypothesis i =
        let test = fromJust (chi2test 1 i)
         in pValue (testSignificance test) > pExpected
   in inp `shouldSatisfy` acceptNullHypothesis

expected ::
  forall f e s.
  (IEEERepr f e s, Integral e) =>
  Unsigned f e s ->
  Unsigned f e s ->
  V.Vector Int
expected a b = V.fromList (map (fromIntegral . weight a b) (fromTo a b))

observed ::
  Int ->
  Unsigned Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  V.Vector Int
observed n a b = V.create $ do
  let u = uniformSigned (toPositive a) b
  v <- MV.new (succ $ distance a b)
  let loop 0 _ = return ()
      loop m gen = do
        let (x, gen') = runState u gen
        MV.modify v succ (distance a (toUnsigned x))
        loop (pred m) gen'
  loop n (mkSMGen 1337)
  return v

distance ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  Int
distance a@(U ea sa) b@(U eb sb)
  | a > b = error "a > b"
  | ea == eb = fromIntegral $ sb - sa
  | otherwise = succ (fromIntegral (m - sa)) + distance (U (succ ea) 0) b
  where
    p = Proxy :: Proxy f
    m = maxSignificand p

next ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s
next (U ea sa)
  | sa == maxSignificand (Proxy :: Proxy f) = U (succ ea) 0
  | otherwise = U ea (succ sa)

fromTo ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  [Unsigned f e s]
fromTo a b
  | a > b = error "a > b"
  | a == b = [a]
  | otherwise = a : fromTo (next a) b

weight ::
  forall f e s.
  (IEEERepr f e s, Integral e) =>
  Unsigned f e s ->
  Unsigned f e s ->
  Unsigned f e s ->
  Word
weight a@(U ea _) b u@(U eu su)
  | a > u || u > b = error "a > u or u > b"
  | a == u = 1
  | b == u && su == 0 = w `div` 4
  | b == u = w `div` 2
  | su == 0 = 3 * (w `div` 4)
  | otherwise = w
  where
    w = (2 :: Word) ^ (succ eu - ea)
