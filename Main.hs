{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception (assert)
import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))

-- TODO: instance IEEERepr, MonadIEEE

isPoint :: RealFloat a => a -> Bool
isPoint f = not (isNaN f) && not (isInfinite f)

assertTrue :: Bool -> Bool
assertTrue = flip assert False

iterateUntilM :: Monad m => m a -> (a -> Bool) -> m a
iterateUntilM f p = f >>= go
  where
    go !x = if p x then return x else f >>= go

-- | Assumption: minBound :: Significand f == 0
class
  (RealFloat f, Eq e, Ord e, Enum e, Eq s, Ord s, Integral s, Bounded s) =>
  IEEERepr f e s
    | f -> e s where
  explode :: f -> (e, s)
  assemble :: (e, s) -> f

class (Monad m, IEEERepr f e s) => MonadIEEE m f e s where
  drawBool :: Proxy f -> m Bool
  drawExponent :: Proxy f -> (e, e) -> m e
  drawSignificand :: Proxy f -> (s, s) -> m s

perhapsNegate :: forall m f e s. MonadIEEE m f e s => m (f -> f)
perhapsNegate = bool id negate <$> drawBool (Proxy :: Proxy f)

-- | [2^e + x, 2^e + y].
uniformExponentsEqual ::
  forall m f e s.
  MonadIEEE m f e s =>
  e ->
  (s, s) ->
  m f
uniformExponentsEqual e (sx, sy) =
  assert (sx < sy) $
    assemble . (e,) <$> drawSignificand (Proxy :: Proxy f) (sx, sy)

-- | [2^e + sx, 2^(e+1) + y].
-- FIXME: this is wrong, consider sx = maxBound; sy = 0
uniformExponentsDifferByOne ::
  forall m f e s.
  MonadIEEE m f e s =>
  e ->
  (s, s) ->
  m f
uniformExponentsDifferByOne e (sx, sy) = assert (sy <= sx `div` 2) $ do
  let maxSMinusSx = maxBound - sx
  r <- drawSignificand (Proxy :: Proxy f) (0, maxSMinusSx + sy + sy)
  return . assemble $
    if r <= maxSMinusSx
      then (e, maxBound - r)
      else (succ e, (r - maxSMinusSx) `div` 2)

-- | [2^x, 2^y]
uniformSignificandsZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, e) ->
  m f
uniformSignificandsZero (ex, ey) = assert (ex < ey) $ do
  let p = Proxy :: Proxy f
  s <- drawSignificand p (0, maxBound)
  e <- drawExponent p (ex, pred ey)
  carry <- ((s == 0) &&) <$> drawBool p
  return $ assemble (if carry then succ e else e, s)

uniformPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformPositive (x, y)
  | assertTrue (isPoint x && isPoint y && 0 <= x && x < y) = error "unreachable"
  | ex == ey = uniformExponentsEqual ex (sx, sy)
  | sx == 0 && sy == 0 = uniformSignificandsZero (ex, ey)
  | succ ex == ey && sy <= sx `div` 2 = uniformExponentsDifferByOne ex (sx, sy)
  | otherwise =
    let sample = uniformSignificandsZero (ex, if sx == 0 then ey else succ ey)
     in sample `iterateUntilM` (\u -> x <= u && u <= y)
  where
    (ex, sx) = explode x
    (ey, sy) = explode y

uniform ::
  forall m f e s.
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniform (x, y)
  | isNaN x || isNaN y = return x
  | x == y = return x
  | x > y = error "uniform"
  | isInfinite x && isInfinite y = bool x y <$> drawBool (Proxy :: Proxy f)
  | isInfinite x = return x
  | isInfinite y = return y
  | y <= 0 = negate <$> uniformRightPositive (negate y, negate x)
  | otherwise = uniformRightPositive (x, y)

uniformRightPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformRightPositive (x, y)
  | assertTrue (isPoint x && isPoint y && x < y && 0 < y) = error "unreachable"
  | 0 <= x = uniformPositive (x, y)
  | negate x == y = perhapsNegate <*> uniformPositive (0, y)
  | otherwise =
    let sample = perhapsNegate <*> uniformPositive (0, max (negate x) y)
     in sample `iterateUntilM` (\u -> x <= u && u <= y)

main :: IO ()
main = putStrLn "Hello, Haskell!"
