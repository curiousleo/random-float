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
import MonadIEEE
  ( MonadIEEE,
    assemble,
    drawBool,
    drawExponent,
    drawSignificand,
    explode,
    perhapsNegate,
  )

isPoint :: RealFloat a => a -> Bool
isPoint f = not (isNaN f) && not (isInfinite f)

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
    sz = max (maxBound - sx) sy
    draw = (,) <$> drawExponent p (ex, ey) <*> drawSignificand p (0, sz)
    go (e, s)
      | assertTrue (e == ex || e == ey) = error "unreachable"
      | s == 0 = do
        e' <- bool e (succ e) <$> drawBool p
        if e' <= ey then return (e', 0) else draw >>= go
      | e == ex && sx <= s' = return (ex, s')
      | e == ey && s <= sy = return (ey, s)
      | otherwise = draw >>= go
      where
        s' = maxBound - s

-- | [2^x, 2^y]
uniformSignificandsZero ::
  forall m f e s.
  MonadIEEE m f e s =>
  (e, e) ->
  m f
uniformSignificandsZero (ex, ey) = assert (succ ex < ey) $ do
  let p = Proxy :: Proxy f
  e <- drawExponent p (ex, pred ey)
  s <- drawSignificand p (0, maxBound)
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
  | succ ex == ey = uniformExponentsDifferByOne ex (sx, sy)
  | otherwise =
    let sample = uniformSignificandsZero (ex, if sx == 0 then ey else succ ey)
     in iterateUntilM (\u -> x <= u && u <= y) sample
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
  | y <= 0 = negate <$> uniformRightPositive (abs y, abs x)
  | otherwise = uniformRightPositive (x, y)

uniformRightPositive ::
  MonadIEEE m f e s =>
  (f, f) ->
  m f
uniformRightPositive (x, y)
  | assertTrue (isPoint x && isPoint y && x < y && 0 < y) = error "unreachable"
  | 0 <= x = uniformPositive (x, y)
  | abs x == y = perhapsNegate <*> uniformPositive (0, y)
  | otherwise =
    let sample = perhapsNegate <*> uniformPositive (0, max (abs x) y)
     in iterateUntilM (\u -> x <= u && u <= y) sample

main :: IO ()
main = putStrLn "Hello, Haskell!"
