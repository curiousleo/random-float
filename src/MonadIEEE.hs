{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MonadIEEE where

import Control.Arrow (first)
import Control.Exception (assert)
import Control.Monad.State.Strict (State, state)
import Data.Bits
import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float
  ( castDoubleToWord64,
    castFloatToWord32,
    castWord32ToFloat,
    castWord64ToDouble,
  )
import System.Random.SplitMix
  ( SMGen,
    bitmaskWithRejection32',
    bitmaskWithRejection64',
    nextWord32,
    nextWord64,
  )

-------------------------------------------------------------------------------
-- IEEERepr and MonadIEEE class declarations
-------------------------------------------------------------------------------

-- | Assumption: minBound :: Significand f == 0
class
  (RealFloat f, Eq e, Ord e, Enum e, Show e, Eq s, Ord s, Integral s, Show s) =>
  IEEERepr f e s
    | f -> e s where
  maxSignificand :: Proxy f -> s
  explode :: f -> (e, s)
  assemble :: (e, s) -> f

class (Monad m, IEEERepr f e s) => MonadIEEE m f e s where
  drawBool :: Proxy f -> m Bool
  drawExponent :: Proxy f -> (e, e) -> m e
  drawSignificand :: Proxy f -> (s, s) -> m s

perhapsNegate :: forall m f e s. MonadIEEE m f e s => m (f -> f)
perhapsNegate = bool id negate <$> drawBool (Proxy :: Proxy f)

-------------------------------------------------------------------------------
-- IEEERepr instances
-------------------------------------------------------------------------------

assertTrue :: Bool -> Bool
assertTrue = flip assert False

mask :: (Bits a, Num a) => Int -> a
mask width = (1 `unsafeShiftL` width) - 1

floatSignificandWidth, floatExponentWidth, floatExponentBias :: Int
floatSignificandWidth = 23
floatExponentWidth = 8
floatExponentBias = 127

instance IEEERepr Float Word8 Word32 where
  maxSignificand _ = mask floatSignificandWidth
  explode f = (fromIntegral e, s)
    where
      w = castFloatToWord32 f
      e = (w `unsafeShiftR` floatSignificandWidth) .&. mask floatExponentWidth
      s = w .&. mask floatSignificandWidth
  assemble (e, s)
    | assertTrue (s' == s) = error "unreachable"
    | otherwise = castWord32ToFloat w
    where
      s' = s .&. mask floatSignificandWidth
      w = (fromIntegral e `unsafeShiftL` floatSignificandWidth) .|. s'

doubleSignificandWidth, doubleExponentWidth, doubleExponentBias :: Int
doubleSignificandWidth = 52
doubleExponentWidth = 11
doubleExponentBias = 1023

instance IEEERepr Double Word16 Word64 where
  maxSignificand _ = mask doubleSignificandWidth
  explode f = (fromIntegral e, s)
    where
      w = castDoubleToWord64 f
      e = (w `unsafeShiftR` doubleSignificandWidth) .&. mask doubleExponentWidth
      s = w .&. mask doubleSignificandWidth
  assemble (e, s)
    | assertTrue (e == e' && s' == s) = error "unreachable"
    | otherwise = castWord64ToDouble w
    where
      e' = e .&. mask doubleExponentWidth
      s' = s .&. mask doubleSignificandWidth
      w = (fromIntegral e' `unsafeShiftL` doubleSignificandWidth) .|. s'

-------------------------------------------------------------------------------
-- MonadIEEE instance for SMGen
-------------------------------------------------------------------------------

drawDownExponent :: Integral a => (a, a) -> (g -> (Word64, g)) -> g -> (Int, g)
drawDownExponent (limit, start) gen = go (fromIntegral start)
  where
    limit' = fromIntegral limit
    go !acc g
      | acc <= limit' = (limit', g)
      | otherwise =
        let (w, g') = gen g
         in if w /= 0
              then (max limit' (acc - countLeadingZeros w), g')
              else go (acc - finiteBitSize w) g

instance MonadIEEE (State SMGen) Float Word8 Word32 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ (x, y) =
    state (first (+ x) . bitmaskWithRejection32' (y - x))
  drawExponent _ (x, y) =
    state (first fromIntegral . drawDownExponent (x, y) nextWord64)

instance MonadIEEE (State SMGen) Double Word16 Word64 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ (x, y) =
    state (first (+ x) . bitmaskWithRejection64' (y - x))
  drawExponent _ (x, y) =
    state (first fromIntegral . drawDownExponent (x, y) nextWord64)
