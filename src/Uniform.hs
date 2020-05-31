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

iterateUntilM :: Monad m => (a -> m Bool) -> m a -> m a
iterateUntilM p f = f >>= go
  where
    go !x = p x >>= bool (f >>= go) (return x)

inInterval ::
  forall m f e s.
  MonadIEEE m f e s =>
  (f, f) ->
  f ->
  m Bool
inInterval (x, y) f
  -- | x <= f && f <= y = return True
  -- | otherwise = return False
  | x < f && f < y = return True
  | x > f || f > y = return False
  | otherwise = drawBool (Proxy :: Proxy f)

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
    -- TODO: use Downey trick to increase zero weight from lowest
    ey = succ ex
    dsx = maxSignificand p - sx
    sz = max dsx sy
    draw = (,,) <$> drawBool p <*> drawBool p <*> drawSignificand p (0, sz)
    -- 'go' returns at a ratio 1 : 2 : 3 : 4 : 2 floats of the form
    --
    --     (ex, sx) : (ex, sx < s) : (ey, 0) : (ey, 0 < s < sy) : (ey, sy)
    --
    -- Imagine that we start with weight 8 and that each coin "reveal" halves
    -- the weight.
    --
    -- Clause 1 outputs (ey, 0) with weight 2 (two coin observations).
    -- Clause 2 outputs (ey, sy) with weight 2 (two coin observations).
    -- Clause 3 outputs (ey, 0 < s < sy) with weight 4 (one coin observation).
    -- Clause 4a outputs (ex, sx < s) with weight 2 (two coin observations).
    -- Clause 4b outputs (ex, sx) with weight 1 (three coin observations).
    -- Clause 4b outputs (ey, 0) with weight 1 (three coin observations).
    --
    -- This gives the desired ratio of 1 : 2 : (2 + 1) : 4 : 2
    go (True, True, 0) = return (ey, 0)
    go (True, False, 0) = return (ey, sy)
    go (True, _, s) | s < sy = return (ey, s)
    go (False, True, s)
      | s < dsx = r
      | s == dsx = drawBool p >>= bool r (return (ey, 0))
      where
        r = return (ex, maxSignificand p - s)
    go _ = draw >>= go

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

uniformPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformPositive (x, y)
  | assertTrue (isPoint x && isPoint y && z <= x && x < y) = error "unreachable"
  | ex == ey = uniformExponentsEqual ex (sx, sy)
  | sx == 0 && sy == 0 = uniformSignificandsZero (ex, pred ey)
  | succ ex == ey = uniformExponentsDifferByOne ex (sx, sy)
  | otherwise =
    let sample = uniformSignificandsZero (ex, if sx == 0 then ey else succ ey)
     in iterateUntilM (inInterval (x, y)) sample
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
     in iterateUntilM (inInterval (x, y)) sample
  where
    z = zero Proxy
