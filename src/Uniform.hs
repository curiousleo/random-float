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
uniformExponentsEqual e (sx, sy) =
  assert (sx < sy) $
    assemble . (e,) <$> drawSignificand (Proxy :: Proxy f) (sx, sy)

-- | [2^ex + sx, 2^(ex+1) + sy].
uniformExponentsDifferByOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  e ->
  (s, s) ->
  m f
uniformExponentsDifferByOne ex (sx, sy) = assemble <$> (draw >>= go)
  where
    p = Proxy :: Proxy f
    ey = succ ex
    dsx = maxSignificand p - sx
    sz = max dsx sy
    draw = (,,) <$> drawBool p <*> drawBool p <*> drawSignificand p (0, sz)
    -- 'go' returns at a ratio 2 : 3 : 4 floats of the form
    --
    --     (ex, s) : (ey, 0) : (ey, s /= 0)
    --
    -- Imagine that we start with weight 16 and that each coin "reveal" halves
    -- the weight.
    --
    -- Clause 1 outputs (ey, 0) with weight 2 (three coin observations).
    -- Clause 2 outputs (ey, 0) with weight 4 (two coin observations).
    -- Clause 3 outputs (ey, s /= 0) with weight 8 (one coin observation).
    -- Clause 4 outputs (ex, s) with weight 4 (two coin observations).
    --
    -- This gives the desired ratio of 4 : 6 : 8.
    --
    -- Note that clause 2 is logically redundant, but it makes the clauses
    -- orthogonal and thus easier to reason about.
    go (True, True, 0) = drawBool p >>= bool (return (ey, 0)) (draw >>= go)
    go (True, False, 0) = return (ey, 0)
    go (True, _, s) | s <= sy = return (ey, s)
    go (False, True, s) | s <= dsx = return (ex, maxSignificand p - s)
    go _ = draw >>= go

-- | [2^x, 2^y]
uniformSignificandsZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, e) ->
  m f
uniformSignificandsZero (ex, ey) = assert (succ ex < ey) $ do
  let p = Proxy :: Proxy f
  e <- drawExponent p (ex, pred ey)
  s <- drawSignificand p (0, maxSignificand p)
  carry <- ((s == 0) &&) <$> drawBool p
  return $ assemble (if carry then succ e else e, s)

uniformPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformPositive (x, y)
  | assertTrue (isPoint x && isPoint y && z <= x && x < y) = error "unreachable"
  | ex == ey = uniformExponentsEqual ex (sx, sy)
  | sx == 0 && sy == 0 = uniformSignificandsZero (ex, ey)
  | succ ex == ey = uniformExponentsDifferByOne ex (sx, sy)
  | otherwise =
    let sample = uniformSignificandsZero (ex, if sx == 0 then ey else succ ey)
     in iterateUntilM (\u -> x <= u && u <= y) sample
  where
    z = zero Proxy
    (ex, sx) = explode x
    (ey, sy) = explode y

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
