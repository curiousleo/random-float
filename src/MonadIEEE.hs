{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module MonadIEEE where

import Control.Arrow (first)
import Control.Exception (assert)
import Control.Monad.State.Strict (State, state)
import Data.Bits
  ( (.&.),
    (.|.),
    Bits,
    countLeadingZeros,
    unsafeShiftL,
    unsafeShiftR,
  )
import Data.Proxy (Proxy)
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

data Signed f e s = S {sNegative :: Bool, sExponent :: e, sSignificand :: s}
  deriving stock (Eq, Show, Read)

instance (Ord e, Ord s) => Ord (Signed f e s) where
  (S True _ _) <= (S False _ _) = True
  (S False _ea _sa) <= (S True _eb _sb) = False
  (S True ea sa) <= (S True eb sb) = ea > eb || (ea == eb && sa >= sb)
  (S False ea sa) <= (S False eb sb) = ea < eb || (ea == eb && sa <= sb)

data Unsigned f e s = U {uExponent :: e, uSignificand :: s}
  deriving stock (Eq, Show, Read)

instance (Ord e, Ord s) => Ord (Unsigned f e s) where
  (U ea sa) <= (U eb sb) = ea < eb || (ea == eb && sa <= sb)
  {-# INLINE (<=) #-}

toUnsigned :: Signed f e s -> Unsigned f e s
toUnsigned S {sExponent, sSignificand} = U sExponent sSignificand

toPositive, toNegative :: Unsigned f e s -> Signed f e s
toPositive U {uExponent, uSignificand} = S False uExponent uSignificand
toNegative U {uExponent, uSignificand} = S True uExponent uSignificand

-- | Assumption: minBound :: Significand f == 0
class
  ( Eq f,
    Ord f,
    Eq e,
    Ord e,
    Enum e,
    Show e,
    Num e,
    Eq s,
    Ord s,
    Integral s,
    Show s
  ) =>
  IEEERepr f e s
    | f -> e s where
  zero :: Unsigned f e s
  maxSignificand :: Proxy f -> s
  explode :: f -> Signed f e s
  assemble :: Signed f e s -> f

class (Monad m, IEEERepr f e s) => MonadIEEE m f e s where
  drawBool :: Proxy f -> m Bool
  drawExponent :: Proxy f -> e -> m e
  drawSignificand :: Proxy f -> s -> m s

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
  zero = U 0 0
  maxSignificand _ = mask floatSignificandWidth
  explode f = S sNegative (fromIntegral sExponent) sSignificand
    where
      w = castFloatToWord32 f
      sNegative = 0 /= w `unsafeShiftR` (floatSignificandWidth + floatExponentWidth)
      sExponent = (w `unsafeShiftR` floatSignificandWidth) .&. mask floatExponentWidth
      sSignificand = w .&. mask floatSignificandWidth
  assemble S {sNegative, sExponent, sSignificand}
    | assertTrue (sSignificand' == sSignificand) = error "unreachable"
    | otherwise = castWord32ToFloat (sNegative' .|. sExponent' .|. sSignificand')
    where
      sNegative' =
        if sNegative
          then 1 `unsafeShiftL` (floatSignificandWidth + floatExponentWidth)
          else 0
      sExponent' = fromIntegral sExponent `unsafeShiftL` floatSignificandWidth
      sSignificand' = sSignificand .&. mask floatSignificandWidth

doubleSignificandWidth, doubleExponentWidth, doubleExponentBias :: Int
doubleSignificandWidth = 52
doubleExponentWidth = 11
doubleExponentBias = 1023

instance IEEERepr Double Word16 Word64 where
  zero = U 0 0
  maxSignificand _ = mask doubleSignificandWidth
  explode f = S sNegative (fromIntegral sExponent) sSignificand
    where
      w = castDoubleToWord64 f
      sNegative = 0 /= w `unsafeShiftR` (doubleSignificandWidth + doubleExponentWidth)
      sExponent = (w `unsafeShiftR` doubleSignificandWidth) .&. mask doubleExponentWidth
      sSignificand = w .&. mask doubleSignificandWidth
  assemble S {sNegative, sExponent, sSignificand}
    | assertTrue (sSignificand' == sSignificand) = error "unreachable"
    | otherwise = castWord64ToDouble (sNegative' .|. sExponent' .|. sSignificand')
    where
      sNegative' =
        if sNegative
          then 1 `unsafeShiftL` (doubleSignificandWidth + doubleExponentWidth)
          else 0
      sExponent' = fromIntegral sExponent `unsafeShiftL` doubleSignificandWidth
      sSignificand' = sSignificand .&. mask doubleSignificandWidth

------------------ Testing ----------------------------

newtype Binary8 = Binary8 {unBinary8 :: Word8}
  deriving newtype (Eq)

instance Show Binary8 where
  show (Binary8 0) = "0"
  show x = show (toFloat x)

toFloat :: Binary8 -> Float
toFloat (Binary8 0) = 0
toFloat x = assemble (S sNegative sExponent' sSignificand')
  where
    S {sNegative, sExponent, sSignificand} = explode x
    sExponent' =
      if sExponent == 0
        then 0
        else sExponent + fromIntegral (floatExponentBias - binary8ExponentBias)
    sSignificand' = fromIntegral sSignificand `unsafeShiftL` (floatSignificandWidth - binary8SignificandWidth)

instance Ord Binary8 where
  x@(Binary8 wx) <= y@(Binary8 wy)
    | xNegative && not yNegative = True
    | not xNegative && not yNegative = wx <= wy
    | xNegative && yNegative = positive y <= positive x
    | otherwise = False
    where
      m = 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
      xNegative = 0 /= m .&. wx
      yNegative = 0 /= m .&. wy
      positive = assemble . toPositive . toUnsigned . explode
  {-# INLINE (<=) #-}

binary8SignificandWidth, binary8ExponentWidth, binary8ExponentBias :: Int
binary8SignificandWidth = 3
binary8ExponentWidth = 4
binary8ExponentBias = 7

instance IEEERepr Binary8 Word8 Word8 where
  zero = U 0 0
  maxSignificand _ = mask binary8SignificandWidth
  explode (Binary8 w) = S {sNegative, sExponent, sSignificand}
    where
      sNegative = 0 /= w `unsafeShiftR` (binary8SignificandWidth + binary8ExponentWidth)
      sExponent = (w `unsafeShiftR` binary8SignificandWidth) .&. mask binary8ExponentWidth
      sSignificand = w .&. mask binary8SignificandWidth
  assemble S {sNegative, sExponent, sSignificand}
    | assertTrue (sSignificand' == sSignificand) = error "unreachable"
    | otherwise = Binary8 (sNegative' .|. sExponent' .|. sSignificand')
    where
      sNegative' =
        if sNegative
          then 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
          else 0
      sExponent' = sExponent `unsafeShiftL` binary8SignificandWidth
      sSignificand' = sSignificand .&. mask binary8SignificandWidth

-------------------------------------------------------------------------------
-- MonadIEEE instance for SMGen
-------------------------------------------------------------------------------

drawDownExponent :: Integral a => a -> (g -> (Word64, g)) -> g -> (Int, g)
drawDownExponent start gen = go start'
  where
    start' = fromIntegral start
    go !acc g
      | acc' < 0 = go start' g'
      | w /= 0 = (acc', g')
      | otherwise = go acc' g'
      where
        (w, g') = gen g
        acc' = acc - countLeadingZeros w

instance MonadIEEE (State SMGen) Float Word8 Word32 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ x = state (bitmaskWithRejection32' x)
  drawExponent _ x = state (first fromIntegral . drawDownExponent x nextWord64)

instance MonadIEEE (State SMGen) Double Word16 Word64 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ x = state (bitmaskWithRejection64' x)
  drawExponent _ x = state (first fromIntegral . drawDownExponent x nextWord64)

instance MonadIEEE (State SMGen) Binary8 Word8 Word8 where
  drawBool _ = state (first (\w -> 0 /= 1 .&. w) . nextWord32)
  drawSignificand _ x = state (first fromIntegral . bitmaskWithRejection32' x')
    where
      x' = fromIntegral x
  drawExponent _ x = state (first fromIntegral . drawDownExponent x nextWord64)
