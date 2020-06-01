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

-- | RealFloat is just too big!
class IEEEFloat a where
  zero :: Proxy a -> a
  isNaN' :: a -> Bool
  isInfinite' :: a -> Bool
  abs' :: a -> a
  negate' :: a -> a

data Signed f e s = S {sNegative :: Bool, sExponent :: e, sSignificand :: s}
  deriving stock (Eq, Show, Read)

instance (Ord e, Ord s) => Ord (Signed f e s) where
  (S True _ _) <= (S False _ _) = True
  (S False ea sa) <= (S True eb sb) = False
  (S True ea sa) <= (S True eb sb) = ea > eb || (ea == eb && sa >= sb)
  (S False ea sa) <= (S False eb sb) = ea < eb || (ea == eb && sa <= sb)

data Unsigned f e s = U {uExponent :: e, uSignificand :: s}
  deriving stock (Eq, Show, Read)

instance (Ord e, Ord s) => Ord (Unsigned f e s) where
  (U ea sa) <= (U eb sb) = ea < eb || (ea == eb && sa <= sb)

toUnsigned :: Signed f e s -> Unsigned f e s
toUnsigned S {sExponent, sSignificand} = U sExponent sSignificand

toPositive, toNegative :: Unsigned f e s -> Signed f e s
toPositive U {uExponent, uSignificand} = S False uExponent uSignificand
toNegative U {uExponent, uSignificand} = S True uExponent uSignificand

-- | Assumption: minBound :: Significand f == 0
class
  ( Eq f,
    Ord f,
    IEEEFloat f,
    Eq e,
    Ord e,
    Enum e,
    Show e,
    Eq s,
    Ord s,
    Integral s,
    Show s
  ) =>
  IEEERepr f e s
    | f -> e s where
  maxSignificand :: Proxy f -> s
  explode :: f -> Signed f e s
  assemble :: Signed f e s -> f

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

instance IEEEFloat Float where
  zero _ = 0
  isNaN' = isNaN
  isInfinite' = isInfinite
  abs' = abs
  negate' = negate

instance IEEERepr Float Word8 Word32 where
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

instance IEEEFloat Double where
  zero _ = 0
  isNaN' = isNaN
  isInfinite' = isInfinite
  abs' = abs
  negate' = negate

instance IEEERepr Double Word16 Word64 where
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
toFloat x@(Binary8 w) = assemble (S sNegative sExponent' sSignificand')
  where
    S {sNegative, sExponent, sSignificand} = explode x
    sExponent' =
      if sExponent == 0
        then 0
        else fromIntegral sExponent + fromIntegral (floatExponentBias - binary8ExponentBias)
    sSignificand' = fromIntegral sSignificand `shiftL` (floatSignificandWidth - binary8SignificandWidth)

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

instance IEEEFloat Binary8 where
  zero _ = Binary8 0
  isNaN' _ = False -- TODO
  isInfinite' _ = False -- TODO
  abs' = assemble . explode
  negate' (Binary8 w) = Binary8 w'
    where
      sign = 1 `unsafeShiftL` (binary8SignificandWidth + binary8ExponentWidth)
      w' = sign `xor` w

instance IEEERepr Binary8 Word8 Word8 where
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
      sExponent' = fromIntegral sExponent `unsafeShiftL` binary8SignificandWidth
      sSignificand' = sSignificand .&. mask binary8SignificandWidth

-------------------------------------------------------------------------------
-- MonadIEEE instance for SMGen
-------------------------------------------------------------------------------

drawDownExponent :: Integral a => (a, a) -> (g -> (Word64, g)) -> g -> (Int, g)
drawDownExponent (limit, start) gen = go start'
  where
    start' = fromIntegral start
    limit' = fromIntegral limit
    go !acc g
      | acc < limit' = go start' g
      | otherwise =
        let (w, g') = gen g
            acc' = acc - countLeadingZeros w
         in if w /= 0
              then if acc' < limit' then go start' g' else (acc', g')
              else go acc' g

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
