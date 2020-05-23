{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception (assert)
import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))
import Prelude hiding (exponent, significand)

-- TODO: instance IEEERepr, MonadIEEE

isPoint :: RealFloat a => a -> Bool
isPoint f = not (isNaN f) && not (isInfinite f)

assertTrue :: Bool -> Bool
assertTrue = flip assert False

unreachable :: a
unreachable = error "unreachable"

class
  ( RealFloat f,
    Eq (Exponent f),
    Enum (Exponent f),
    Eq (Significand f),
    Ord (Significand f),
    Integral (Significand f)
  ) =>
  IEEERepr f where
  type Exponent f
  type Significand f

  zeroExponent :: Proxy f -> Exponent f
  maxSignificand :: Proxy f -> Significand f

  explode :: f -> (Exponent f, Significand f)
  assemble :: Proxy f -> (Exponent f, Significand f) -> f

class (Monad m, IEEERepr f) => MonadIEEE m f where
  drawBool :: Proxy f -> m Bool
  drawExponent :: Proxy f -> (Exponent f, Exponent f) -> m (Exponent f)
  drawSignificand ::
    Proxy f ->
    (Significand f, Significand f) ->
    m (Significand f)

perhapsNegate :: forall f m. MonadIEEE m f => m (f -> f)
perhapsNegate = bool id negate <$> drawBool (Proxy :: Proxy f)

-- | [2^e + x, 2^e + y].
uniformExponentsEqual ::
  MonadIEEE m f =>
  Proxy f ->
  Exponent f ->
  Significand f ->
  Significand f ->
  m f
uniformExponentsEqual p e sx sy =
  assemble p . (e,) <$> drawSignificand p (sx, sy)

-- | [2^e + sx, 2^(e+1) + y].
uniformExponentsDifferByOne ::
  MonadIEEE m f =>
  Proxy f ->
  Exponent f ->
  Significand f ->
  Significand f ->
  m f
uniformExponentsDifferByOne p e sx sy =
  assert (sy <= maxSignificand p `div` 2) go
  where
    m = max (maxSignificand p - sx) (sy + sy)
    go = do
      b <- drawBool p
      s <- drawSignificand p (0, m)
      if b
        then return $ assemble p (succ e, s `div` 2)
        else
          let sx' = maxSignificand p - s
           in if sx <= sx'
                then return $ assemble p (e, sx')
                else go

-- | [2^x, 2^y]
uniformSignificandsZero ::
  MonadIEEE m f =>
  Proxy f ->
  Exponent f ->
  Exponent f ->
  m f
uniformSignificandsZero p ex ey = do
  s <- drawSignificand p (0, maxSignificand p)
  e <- drawExponent p (ex, pred ey)
  carry <- ((s == 0) &&) <$> drawBool p
  return $ assemble p (bool e (succ e) carry, s)

uniformPositive ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniformPositive x y
  | assertTrue (isPoint x && isPoint y && 0 <= x && x < y) = unreachable
  | ex == ey = uniformExponentsEqual Proxy ex sx sy
  | sx == 0 && sy == 0 = uniformSignificandsZero Proxy ex ey
  | succ ex == ey && sy <= maxS `div` 2 = uniformExponentsDifferByOne p ex sx sy
  | otherwise =
    let go = do
          u <- uniformSignificandsZero Proxy ex ey
          if x <= u && u <= y then return u else go
     in go
  where
    (ex, sx) = explode x
    (ey, sy) = explode y
    maxS = maxSignificand p
    p = Proxy :: Proxy f

uniform ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniform x y
  | isNaN x || isNaN y = return x
  | x == y = return x
  | x > y = error "uniform"
  | isInfinite x && isInfinite y = bool x y <$> drawBool p
  | isInfinite x = return x
  | isInfinite y = return y
  | y <= 0 = negate <$> uniformRightPositive (- y) (- x)
  | otherwise = uniformRightPositive x y
  where
    p = Proxy :: Proxy f

uniformRightPositive ::
  forall f m.
  MonadIEEE m f =>
  f ->
  f ->
  m f
uniformRightPositive x y
  | assertTrue (isPoint x && isPoint y && x < y && 0 < y) = unreachable
  | 0 <= x = uniformPositive x y
  | negate x == y = perhapsNegate <*> uniformPositive 0 y
  | otherwise =
    let z = max (negate x) y
        go = do
          u <- perhapsNegate <*> uniformPositive 0 z
          if x <= u && u <= y then return u else go
     in go

main :: IO ()
main = putStrLn "Hello, Haskell!"
