{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.ST (runST)
import Data.Bits (shiftR)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import MonadIEEE (MonadIEEE (..), toUnsigned, toPositive, IEEERepr (..), Unsigned (..), Binary8)
import Control.Monad (replicateM)
import Statistics.Test.ChiSquared (chi2test)
import Statistics.Test.Types (Test (..))
import Statistics.Types (pValue)
import Test.Hspec
import Test.Validity (identity)
import Uniform (uniformSigned)
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.State.Strict (runState, evalState)
import System.Random.SplitMix (mkSMGen)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word (Word8)

main :: IO ()
main =
  let x = U 0 1 :: Unsigned Binary8 Word8 Word8
      y = U 3 4 :: Unsigned Binary8 Word8 Word8
   in do
     mapM_ (putStrLn . show . pUniform x y) [10,100]

pUniform ::
  Unsigned Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  Int ->
    Double
pUniform x y n =
  let ept = V.map (*n) (expected x y) :: V.Vector Int
      obs = observed' (V.sum ept) x y
      inp = V.zip obs (V.map (fromIntegral :: Int -> Double) ept)
      Just test = chi2test 1 inp
   in pValue (testSignificance test)

isUniform::
  Unsigned Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  Int ->
    Double -> Bool
isUniform x y n p = pUniform x y n > p

observed' ::
  Int ->
  Unsigned Binary8 Word8 Word8 ->
  Unsigned Binary8 Word8 Word8 ->
  V.Vector Int
observed' n a b = V.create $ do
      let u = uniformSigned (toPositive a) b
      v <- MV.new (succ $ distance a b)
      let loop 0 _ = return ()
          loop m gen = do
            let (x, gen') = runState u gen
            MV.modify v succ (distance a (toUnsigned x))
            loop (pred m) gen'
      loop n (mkSMGen 1337)
      return v

-- main = hspec $ do
--   describe "uniform" $ do
--     it "has identity NaN" $ do
--       identity uniform (read "NaN" :: Float)

samples ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  s ->
  s
samples a@(U ea sa) b@(U eb sb) width
  | a >= b = error "a >= b"
  | ea == eb =
    let s = succ (sb - sa)
     in width * s
  | otherwise =
    let s = succ (maxSignificand p - sa)
        a' = U (succ ea) 0
        width' = 2 * width
     in width * s + samples a' b width'
  where
    p = Proxy :: Proxy f

distance ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  Int
distance a@(U ea sa) b@(U eb sb)
  -- | a > b = error "a > b"
  | ea == eb = fromIntegral $ sb - sa
  | otherwise = succ (fromIntegral (m - sa)) + distance (U (succ ea) 0) b
  where
    p = Proxy :: Proxy f
    m = maxSignificand p
{-# INLINE distance #-}

observed ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  [Unsigned f e s] ->
  V.Vector Int
observed a b xs = V.create $ do
  v <- MV.new (succ $ distance a b)
  mapM_ (\x -> MV.modify v succ (distance a x)) xs
  return v

next ::
  forall f e s.
  IEEERepr f e s =>
  Unsigned f e s ->
  Unsigned f e s
next a@(U ea sa)
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
  | b == u = w `div` 2
  | su == 0 = 3 * (w `div` 4)
  | otherwise = w
  where
    w = (2 :: Word) ^ (succ eu - ea)

expected ::
  forall f e s.
    (IEEERepr f e s, Integral e) =>
      Unsigned f e s ->
        Unsigned f e s ->
          V.Vector Int
expected a b = V.fromList (map (fromIntegral . weight a b) (fromTo a b))

  {-
p ::
  forall f e s.
  (IEEERepr f e s, Integral e) =>
  Unsigned f e s ->
  Unsigned f e s ->
  [Unsigned f e s] ->
  Double
p a b xs
  | fromIntegral (V.sum obs) /= V.sum ept = error $ "sum(obs) = " ++ show (V.sum obs) ++ " sum(ept) = " ++ show (V.sum ept)
  | otherwise = pValue (testSignificance test)
  where
    obs = observed a b xs
    ept = V.map (*1000) $ expected a b
    Just test = chi2test 0 (V.zip obs ept)
    -}
