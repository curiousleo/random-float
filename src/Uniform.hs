{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Uniform where

import Control.Exception (assert)
import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))
import Debug.Trace (trace)
import MonadIEEE
  ( MonadIEEE,
    abs',
    Unsigned(..),
    Signed(..),
    assemble,
    toPositive,
    toNegative,
    toUnsigned,
    drawBool,
    drawExponent,
    drawSignificand,
    explode,
    isInfinite',
    isNaN',
    isPoint,
    maxSignificand,
    negate',
    negateUnlessZero,
    zero,
  )

assertTrue :: Bool -> Bool
assertTrue = flip assert False

iterateUntilM :: Monad m => (a -> Bool) -> m a -> m a
iterateUntilM p f = f >>= go
  where
    go !x = if p x then return x else f >>= go

-- | [2^e + sx, 2^e + sy).
uniformExponentsEqual ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
uniformExponentsEqual (U ex sx) (U ey sy) =
  assert (ex == ey && sx < sy)
    $ U e <$> drawSignificand Proxy (sx, pred sy)

-- | [2^ex + sx, 2^(ex+1) + sy).
uniformExponentsDifferByOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
uniformExponentsDifferByOne rx@(U ex sx) ry@(U ey sy) =
  assert (succ ex == ey) $ draw >>= go
  where
    p = Proxy :: Proxy f
    dsx = maxSignificand p - sx
    sz = max dsx sy
    draw = do
      (e, s) <- (,) <$> drawExponent p (ex, ey) <*> drawSignificand p (0, sz)
      return $ U e (if e == ey then s else maxSignificand p - s)
    go u
      | rx <= u && u < ry = return u
      | otherwise = draw >>= go

-- | [2^ex + sx, 2^ey + sy)
uniformPositiveGeneral ::
  forall m f e s.
  MonadIEEE m f e s =>
  Proxy f ->
  (e, s) ->
  (e, s) ->
  m (e, s)
uniformPositiveGeneral rx@(ex, _) ry@(ey, _) =
  assert (rx < ry) $ draw >>= go
  where
    p = Proxy :: Proxy f
    draw =
      (,) <$> drawExponent p (ex, ey)
        <*> drawSignificand p (0, maxSignificand p)
    go u@(e, s)
      | u < rx || ry <= u = draw >>= go
      | otherwise = return u

uniformPositive ::
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
uniformPositive rx@(U ex sx) ry@(U ey sy)
  | ex == ey = uniformExponentsEqual ex (sx, sy)
  | succ ex == ey = uniformExponentsDifferByOne rx ry
  | otherwise = uniformPositiveGeneral rx ry

-- | [-y, y)
uniformSymmetric ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  m (Signed f e s)
uniformSymmetric ry@(U ey sy) = draw >>= go
  where
    p = Proxy :: Proxy f
    rz = toUnsigned . explode (zero p)
    draw = (,) <$> uniformPositive rz ry <*> drawBool p
    go (ru@(e, s), b)
      | b && ru < ry = return . assemble . toPositive ru
      | not b && ru <= ry = return . assemble . toNegative ru
      | otherwise = draw >>= go

redistribute ::
  forall m f e s.
  MonadIEEE m f e s =>
  Unsigned f e s ->
  Unsigned f e s ->
  Unsigned f e s ->
  m (Unsigned f e s)
redistribute p rx ry ru@(eu, su)
  | assertTrue (rx <= ru && ru < ry) = error "unreachable"
  | ru == rx || su == 0 = bool ru (min ry (succ eu, 0)) <$> drawBool p
  | otherwise = return ru

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
  | y <= zero Proxy = negateUnlessZero <$> uniformRightExclusive (abs' y, abs' x)
  | otherwise = uniformRightExclusive (x, y)

uniformRightExclusive ::
  MonadIEEE m f e s =>
  Signed f e s ->
  Unsigned f e s ->
  m (Signed f e s)
uniformRightExclusive (S False ex sx) y =
  assert ((U ex sx) < y) $ toPositive <$> uniformPositive (U ex sx) y
uniformRightExclusive x@(S True ex sx) y =
  let sample = uniformSymmetric (max (U ex sx) y)
   in iterateUntilM (\u -> x <= u && u < toPositive y) sample
