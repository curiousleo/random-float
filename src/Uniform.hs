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
    isInfinite',
    isNaN',
    maxSignificand,
    negateUnlessZero,
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
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniform (x, y)
  | isNaN' x || isNaN' y = return x
  | x == y = return x
  | x > y = error "uniform"
  | isInfinite' x && isInfinite' y = bool x y <$> drawBool (Proxy :: Proxy f)
  | isInfinite' x = return x
  | isInfinite' y = return y
  | y <= zero Proxy =
    negateUnlessZero . assemble
      <$> uniformS (toPositive $ explodeU y) (explodeU x)
  | otherwise = assemble <$> uniformS (explode x) (explodeU y)
  where
    explodeU = toUnsigned . explode
    uniformS x b = uniformXS x b >>= downeyShiftS x b

uniformXS ::
  MonadIEEE m f e s =>
  Signed f e s ->
  Unsigned f e s ->
  m (Signed f e s)
uniformXS x b
  | not (sNegative x) = assert (a < b) $ toPositive <$> uniformXU a b
  | otherwise =
    assert (x < y) $
      iterateUntilM (\u -> x <= u && u < y) (drawSymmetric (max a b))
  where
    a = toUnsigned x
    y = toPositive b

-- | [-b, b). Inexact.
drawSymmetric ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  m (Signed f e s)
drawSymmetric b = bool toPositive toNegative <$> drawBool p <*> uniformXU z b
  where
    p = Proxy :: Proxy f
    z = toUnsigned $ explode $ zero p

-- | [a, b)
uniformXU ::
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
uniformXU a@(U ea _) b@(U eb _)
  | ea == eb = uniformXExponentDiffZero a b
  | succ ea == eb = iterateUntilM inRange $ drawExponentDiffOne a b
  | otherwise = iterateUntilM inRange $ drawExponentDiffGreaterOne a b
  where
    inRange u = a <= u && u < b

-- | [2^e + sx, 2^e + sy)
uniformXExponentDiffZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
uniformXExponentDiffZero (U ea sa) (U eb sb) =
  assert (ea == eb && sa < sb) $
    U ea <$> drawSignificand (Proxy :: Proxy f) (sa, pred sb)

-- | [2^ex + sx, 2^(ex+1) + sy)
drawExponentDiffOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
drawExponentDiffOne (U ea sa) (U eb sb) =
  assert (succ ea == eb) draw
  where
    p = Proxy :: Proxy f
    sz = max (maxSignificand p - sa) sb
    draw = do
      (e, s) <- (,) <$> drawExponent p (ea, eb) <*> drawSignificand p (0, sz)
      return $ U e (if e == eb then s else maxSignificand p - s)

-- | [2^ex + sx, 2^ey + sy)
drawExponentDiffGreaterOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
drawExponentDiffGreaterOne a@(U ea _) b@(U eb _) =
  assert (a < b) $
    U <$> drawExponent p (ea, eb)
      <*> drawSignificand p (0, maxSignificand p)
  where
    p = Proxy :: Proxy f

downeyShiftS ::
  forall m f e s.
  MonadIEEE m f e s =>
  Signed f e s ->
  Unsigned f e s ->
  Signed f e s ->
  m (Signed f e s)
downeyShiftS x b u
  | assertTrue (x <= u && u < toPositive b) = error "unreachable"
  | sNegative u = toNegative <$> downeyShiftU z a v
  | sNegative x = toPositive <$> downeyShiftU z b v
  | otherwise = toPositive <$> downeyShiftU a b v
  where
    a = toUnsigned x
    v = toUnsigned u
    z = toUnsigned $ explode $ zero (Proxy :: Proxy f)

downeyShiftU ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
downeyShiftU a b u@(U eu su)
  | assertTrue (a <= u && u < b) = error "unreachable"
  | u == a || su == 0 = bool u (min b (U (succ eu) 0)) <$> drawBool p
  | otherwise = return u
  where
    p = Proxy :: Proxy f
