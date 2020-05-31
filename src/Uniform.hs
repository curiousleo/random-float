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
    assemble,
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

-- | [2^e + sx, 2^e + sy].
uniformExponentsEqual ::
  forall m f e s.
  MonadIEEE m f e s =>
  e ->
  (s, s) ->
  m f
uniformExponentsEqual e (sx, sy)
  | assertTrue (sx < sy) = error "unreachable"
  | sx == pred sy = assemble . (e,) <$> sxOrSy
  | otherwise = do
    s <- drawSignificand p (sx, pred sy)
    assemble . (e,) <$> if s == sx then sxOrSy else return s
  where
    p = Proxy :: Proxy f
    sxOrSy = bool sx sy <$> drawBool p

-- | [2^ex + sx, 2^(ex+1) + sy].
uniformExponentsDifferByOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, s) ->
  (e, s) ->
  m f
uniformExponentsDifferByOne (ex, 0) (ey, 0) =
  assert (succ ex == ey) $
    assemble . (,0) <$> drawExponent (Proxy :: Proxy f) (ex, succ ex)
uniformExponentsDifferByOne rx@(ex, sx) ry@(ey, sy)
  | assertTrue (succ ex == ey) = error "unreachable"
  | dsx <= sy = assemble <$> (draw >>= go rx ry)
  | otherwise = assemble <$> (draw >>= go ry rx)
  where
    p = Proxy :: Proxy f
    c = (ey, 0)
    dsx = maxSignificand p - sx
    sz = pred (max dsx sy)
    draw = do
      (e, s) <- (,) <$> drawExponent p (ex, ey) <*> drawSignificand p (0, sz)
      return (e, if e == ey then s else maxSignificand p - s)
    go from to u
      | u == from = bool from c <$> drawBool p
      | u == c = bool c to <$> drawBool p
      | rx <= u && u <= ry = return u
      | otherwise = draw >>= go from to

-- | [2^x, 2^y]
uniformSignificandsZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, e) ->
  m f
uniformSignificandsZero (ex, ey) = assert (ex < ey) $ do
  let p = Proxy :: Proxy f
  e <- drawExponent p (ex, ey)
  s <- drawSignificand p (0, maxSignificand p)
  carry <- ((s == 0) &&) <$> drawBool p
  return $ assemble (if carry then succ e else e, s)

uniformPositiveGeneral ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, s) ->
  (e, s) ->
  m f
uniformPositiveGeneral rx@(ex, sx) ry@(ey, sy) =
  assert (rx < ry) $ assemble <$> (draw >>= go)
  where
    p = Proxy :: Proxy f
    draw =
      (,) <$> drawExponent p (ex, ey)
        <*> drawSignificand p (0, maxSignificand p)
    go u@(e, s)
      | u < rx || ry <= u = draw >>= go
      | u == rx || s == 0 = bool u (min ry (succ e, 0)) <$> drawBool p
      | otherwise = return u

uniformPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformPositive (x, y)
  | assertTrue (isPoint x && isPoint y && z <= x && x < y) = error "unreachable"
  | ex == ey = uniformExponentsEqual ex (sx, sy)
  | sx == 0 && sy == 0 = uniformSignificandsZero (ex, pred ey)
  | succ ex == ey = uniformExponentsDifferByOne rx ry
  | otherwise = uniformPositiveGeneral rx ry
  where
    z = zero Proxy
    rx@(ex, sx) = explode x
    ry@(ey, sy) = explode y

uniformSymmetric ::
  forall m f e s.
  MonadIEEE m f e s =>
  f ->
  m f
uniformSymmetric x = do
  let z = zero Proxy
  f <- uniformPositive (z, x)
  b <- drawBool (Proxy :: Proxy f)
  if f == z && b
    then uniformSymmetric x
    else return $ if b then negate' f else f

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
  | y <= zero Proxy = negateUnlessZero <$> uniformRightPositive (abs' y, abs' x)
  | otherwise = uniformRightPositive (x, y)

uniformRightPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformRightPositive (x, y)
  | assertTrue (isPoint x && isPoint y && x < y && z < y) = error "unreachable"
  | z <= x = uniformPositive (x, y)
  | abs' x == y = uniformSymmetric y
  | otherwise =
    let sample = uniformSymmetric (max (abs' x) y)
     in iterateUntilM (\u -> x <= u && u <= y) sample
  where
    z = zero Proxy
