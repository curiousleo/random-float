{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Uniform where

import Control.Exception (assert)
import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))
import MonadIEEE
  ( MonadIEEE,
    Signed (..),
    Unsigned (..),
    assemble,
    drawBool,
    drawExponent,
    drawSignificand,
    explode,
    maxSignificand,
    toNegative,
    toPositive,
    toUnsigned,
    zero,
  )

assertTrue :: Bool -> Bool
assertTrue = flip assert False

iterateUntilM :: Monad m => (a -> Bool) -> m a -> m a
iterateUntilM p f = f >>= go
  where
    go !x = if p x then return x else f >>= go

uniform ::
  forall m f e s.
  (RealFloat f, MonadIEEE m f e s) =>
  (f, f) ->
  m f
uniform (x, y)
  | isNaN x || isNaN y = return x
  | x == y = return x
  | x > y = error "uniform"
  | isInfinite x && isInfinite y = bool x y <$> drawBool (Proxy :: Proxy f)
  | isInfinite x = return x
  | isInfinite y = return y
  | y <= assemble (toPositive zero) =
    negate . assemble
      <$> uniformSigned (toPositive $ explodeUnsigned y) (explodeUnsigned x)
  | otherwise = assemble <$> uniformSigned (explode x) (explodeUnsigned y)
  where
    explodeUnsigned = toUnsigned . explode
    uniformSigned x b = drawSigned x b >>= downeyShiftSigned x b

drawSigned ::
  MonadIEEE m f e s =>
  Signed f e s ->
  Unsigned f e s ->
  m (Signed f e s)
drawSigned x b
  | not (sNegative x) =
    assert (a < b) $
      toPositive <$> iterateUntilM (\u -> a <= u && u < b) (proposeUnsigned a b)
  | otherwise =
    assert (x < y) $
      iterateUntilM (\u -> x < u && u < y) (proposeSymmetric (max a b))
  where
    a = toUnsigned x
    y = toPositive b

-- | [-b, b]
proposeSymmetric ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  m (Signed f e s)
proposeSymmetric b =
  bool toPositive toNegative <$> drawBool p <*> proposeUnsigned zero b
  where
    p = Proxy :: Proxy f

-- | [a, b)
proposeUnsigned ::
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
proposeUnsigned a@(U ea _) b@(U eb _)
  | ea == eb = drawExponentDiffZero a b
  | succ ea == eb = proposeExponentDiffOne a b
  | otherwise = proposeExponentDiffGreaterOne a b

-- | [2^e + sx, 2^e + sy)
drawExponentDiffZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
drawExponentDiffZero (U ea sa) (U eb sb) =
  assert (ea == eb && sa < sb) $
    U ea <$> ((sa +) <$> drawSignificand (Proxy :: Proxy f) (pred sb - sa))

-- | [2^ex + sx, 2^(ex+1) + sy)
proposeExponentDiffOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
proposeExponentDiffOne (U ea sa) (U eb sb) =
  assert (succ ea == eb) $ do
    (e, s) <- (,) <$> ((ea +) <$> drawExponent p 1) <*> drawSignificand p sz
    return $ U e (if e == eb then s else maxSignificand p - s)
  where
    p = Proxy :: Proxy f
    sz = max (min sb (pred sb)) (maxSignificand p - sa)

-- | [2^ex + sx, 2^ey + sy)
proposeExponentDiffGreaterOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
proposeExponentDiffGreaterOne (U ea _) (U eb _) =
  assert (succ ea < eb) $
    U <$> ((ea +) <$> drawExponent p (eb - ea))
      <*> drawSignificand p (maxSignificand p)
  where
    p = Proxy :: Proxy f

-- | @[x, b)@ to @[x, b]@
downeyShiftSigned ::
  forall m f e s.
  MonadIEEE m f e s =>
  Signed f e s ->
  Unsigned f e s ->
  Signed f e s ->
  m (Signed f e s)
downeyShiftSigned x b u
  | sNegative u = toNegative <$> downeyShiftUnsigned zero a v
  | sNegative x = toPositive <$> downeyShiftUnsigned zero b v
  | otherwise = toPositive <$> downeyShiftUnsigned a b v
  where
    a = toUnsigned x
    v = toUnsigned u

-- | [a, b) to [a, b]
downeyShiftUnsigned ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
downeyShiftUnsigned a b u@(U eu su)
  | assertTrue (a <= u && u < b) = error "unreachable"
  | u == a || su == 0 = bool u (min b (U (succ eu) 0)) <$> drawBool p
  | otherwise = return u
  where
    p = Proxy :: Proxy f
