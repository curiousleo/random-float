{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
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

negateUnlessZero :: IEEERepr f e s => f -> f
negateUnlessZero x = if abs' x == zero Proxy then x else negate' x

-------------------------------------------------------------------------------
-- IEEERepr and MonadIEEE class declarations
-------------------------------------------------------------------------------

-- | Assumption: minBound :: Significand f == 0
class
  (Eq f, Ord f, Eq e, Ord e, Enum e, Show e, Eq s, Ord s, Integral s, Show s) =>
  IEEERepr f e s
    | f -> e s where
  isNaN' :: f -> Bool
  isInfinite' :: f -> Bool

  zero :: Proxy f -> f
  maxSignificand :: Proxy f -> s

  abs' :: f -> f
  negate' :: f -> f
  explode :: f -> (e, s)
  assemble :: (e, s) -> f

class (Monad m, IEEERepr f e s) => MonadIEEE m f e s where
  drawBool :: Proxy f -> m Bool
  drawExponent :: Proxy f -> (e, e) -> m e
  drawSignificand :: Proxy f -> (s, s) -> m s

isPoint :: IEEERepr f e s => f -> Bool
isPoint f = not (isNaN' f) && not (isInfinite' f)

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
  isNaN' = isNaN
  isInfinite' = isInfinite
  zero _ = 0
  maxSignificand _ = mask floatSignificandWidth
  abs' = abs
  negate' = negate
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
  isNaN' = isNaN
  isInfinite' = isInfinite
  zero _ = 0
  maxSignificand _ = mask doubleSignificandWidth
  abs' = abs
  negate' = negate
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

------------------ Testing ----------------------------

newtype Binary8 = Binary8 {unBinary8 :: Word8}
  deriving newtype (Eq)

instance Show Binary8 where
  show x@(Binary8 w) = show $ (bool id negate neg) (s' * (2::Float) ^^ e')
    where
      m = 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
      neg = 0 /= m .&. w
      (e, s) = explode x
      e' = (fromIntegral e :: Int) - binary8ExponentBias
      maxS = maxSignificand (Proxy :: Proxy Binary8)
      s' = (fromIntegral (s .|. (1 `shiftL` binary8SignificandWidth)) :: Float) / succ (fromIntegral maxS)

instance Ord Binary8 where
  x@(Binary8 wx) <= y@(Binary8 wy)
    | xNegative && not yNegative = True
    | not xNegative && not yNegative = wx <= wy
    | xNegative && yNegative = negate' y <= negate' x
    | otherwise = False
    where
      m = 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
      xNegative = 0 /= m .&. wx
      yNegative = 0 /= m .&. wy

binary8SignificandWidth, binary8ExponentWidth, binary8ExponentBias :: Int
binary8SignificandWidth = 3
binary8ExponentWidth = 4
binary8ExponentBias = 7

instance IEEERepr Binary8 Word8 Word8 where
  isNaN' _ = False -- TODO
  isInfinite' _ = False -- TODO
  zero _ = Binary8 0
  maxSignificand _ = mask binary8SignificandWidth
  abs' = assemble . explode
  negate' (Binary8 w) = Binary8 w'
    where
      sign = 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
      w' = sign `xor` w
  explode (Binary8 w) = (fromIntegral e, s)
    where
      e = (w `unsafeShiftR` binary8SignificandWidth) .&. mask binary8ExponentWidth
      s = w .&. mask binary8SignificandWidth
  assemble (e, s)
    | assertTrue (s' == s) = error "unreachable"
    | otherwise = Binary8 w
    where
      s' = s .&. mask binary8SignificandWidth
      w = (fromIntegral e `unsafeShiftL` binary8SignificandWidth) .|. s'
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

instance MonadIEEE (State SMGen) Binary8 Word8 Word8 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ (x, y) =
    state (first ((+ x) . fromIntegral) . bitmaskWithRejection32' d)
      where
        d = fromIntegral (y - x)
  drawExponent _ (x, y) =
    state (first fromIntegral . drawDownExponent (x, y) nextWord64)
