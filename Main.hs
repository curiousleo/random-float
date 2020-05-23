{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))
import Prelude hiding (exponent, significand)

-- TODO: instance IEEERepr, MonadIEEE

class
  ( RealFloat f,
    Eq (Exponent f),
    Eq (Significand f),
    Num (Exponent f),
    Num (Significand f)
  ) =>
  IEEERepr f where
  type Exponent f
  type Significand f

  zeroExponent :: Proxy f -> Exponent f

  minSignificand :: Proxy f -> Significand f
  maxSignificand :: Proxy f -> Significand f

  exponent :: f -> Exponent f
  significand :: f -> Significand f

  assemblePositive :: Proxy f -> (Exponent f, Significand f) -> f

class (Monad m, IEEERepr f) => MonadIEEE m f where
  drawSign :: Proxy f -> m Bool
  drawExponent :: Proxy f -> (Exponent f, Exponent f) -> m (Exponent f)
  drawSignificand ::
    Proxy f ->
    (Significand f, Significand f) ->
    m (Significand f)

perhapsNegate :: forall f m. MonadIEEE m f => m (f -> f)
perhapsNegate = bool id negate <$> drawSign (Proxy :: Proxy f)

-- | [2^e + x, 2^e + y].
uniformExponentsEqual ::
  MonadIEEE m f =>
  Proxy f ->
  Exponent f ->
  Significand f ->
  Significand f ->
  m f
uniformExponentsEqual p e sx sy =
  assemblePositive p . (e,) <$> drawSignificand p (sx, sy)

-- | [2^x, 2^y]
uniformSignificandsZero ::
  MonadIEEE m f =>
  Proxy f ->
  Exponent f ->
  Exponent f ->
  m f
uniformSignificandsZero p ex ey = do
  e <- drawExponent p (ex, ey)
  s <- drawSignificand p (minSignificand p, maxSignificand p)
  return $ assemblePositive p (e, s)

-- | [x, y], assumes 0 < x < y.
uniformPositive ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniformPositive x y
  | ex == ey = uniformExponentsEqual Proxy ex sx sy
  | sx == 0 && sy == 0 = uniformSignificandsZero Proxy ex ey
  | otherwise =
    let go = do
          u <- uniformSignificandsZero Proxy ex ey
          if x <= u && u <= y then return u else go
     in go
  where
    ex = exponent x
    ey = exponent y
    sx = significand x
    sy = significand y

-- | [x, y], assumes x < 0 < y.
uniformSpansZero ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniformSpansZero x y = go
  where
    z = max (- x) y
    go = do
      u <- perhapsNegate <*> uniformPositive 0 z
      if x <= u && u <= y then return u else go

uniform ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniform x y
  | isNaN x || isNaN y = return x
  | x == y = return x
  | x > y = error "uniform: requires not y < x"
  | isInfinite x && isInfinite y = bool x y <$> drawSign p
  | isInfinite x = return x
  | isInfinite y = return y
  | y <= 0 = negate <$> uniformRightPositive (- y) (- x)
  | otherwise = uniformRightPositive x y
  where
    p = Proxy :: Proxy f

-- | Assumes x < y and y > 0.
uniformRightPositive ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniformRightPositive x y
  | x == - y = perhapsNegate <*> uniformPositive 0 y
  | x < 0 = uniformSpansZero x y
  | otherwise = uniformPositive x y

main :: IO ()
main = putStrLn "Hello, Haskell!"
