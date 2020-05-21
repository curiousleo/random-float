{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

class
  ( RealFloat f,
    Eq (Exponent f),
    Num (Significand f),
    Eq (Significand f)
  ) =>
  IEEERepr f where
  type Exponent f
  type Significand f

  nan :: f
  inf :: f

  expt :: f -> Exponent f
  sfcd :: f -> Significand f

data Interval f
  = NaNEndpoint
  | Degenerate f
  | LeftInf
  | RightInf
  | BothInf
  | Finite f f

-- | Assumes that the right endpoint is positive
data FiniteInterval f
  = LeftZero f
  | Symmetric f
  | Positive f f
  | SpansZero f f

data LeftZeroInterval f
  = SignificandZero (Exponent f)
  | LeftZeroIntervalOther f

data PositiveInterval f
  = ExponentsEqual (Exponent f) (Significand f) (Significand f)
  | SignificandsZero (Exponent f) (Exponent f)
  | PositiveIntervalOther f f

classify ::
  RealFloat f =>
  (f, f) ->
  Interval f
classify (x, y)
  | y < x = error "classify: requires not y < x"
  | isNaN x || isNaN y = NaNEndpoint
  | x == y = Degenerate x
  | isInfinite x && isInfinite y = BothInf
  | isInfinite x = LeftInf
  | isInfinite y = RightInf
  | otherwise = Finite x y

endpoints ::
  IEEERepr f =>
  Interval f ->
  (Maybe f, Maybe f)
endpoints NaNEndpoint = (Nothing, Nothing)
endpoints (Degenerate x) = (Just x, Just x)
endpoints BothInf = (Just (- inf), Just inf)
endpoints LeftInf = (Just (- inf), Nothing)
endpoints RightInf = (Nothing, Just inf)
endpoints (Finite x y) = (Just x, Just y)

uniformExponentsEqual ::
  IEEERepr f =>
  Exponent f ->
  Significand f ->
  Significand f ->
  f
uniformExponentsEqual = undefined

uniformSignificandsZero ::
  IEEERepr f =>
  Exponent f ->
  Exponent f ->
  f
uniformSignificandsZero = undefined

uniformLeftZero ::
  IEEERepr f =>
  f ->
  f
uniformLeftZero = undefined

-- | Assumes 0 < x < y.
uniformPositive ::
  IEEERepr f =>
  f ->
  f ->
  f
uniformPositive = undefined

-- | Assumes x < 0 < y.
uniformSpansZero ::
  f ->
  f ->
  f
uniformSpansZero = undefined

uniform' ::
  IEEERepr f =>
  Interval f ->
  f
uniform' NaNEndpoint = nan
uniform' (Degenerate x) = x
uniform' LeftInf = - inf
uniform' RightInf = inf
uniform' BothInf = inf -- TODO: flip coin for sign
uniform' (Finite x y)
  | y <= 0 = - (uniformRightPositive (- y) (- x))
  | otherwise = uniformRightPositive x y

uniform ::
  IEEERepr f =>
  f ->
  f ->
  f
uniform x y
  | isNaN x || isNaN y = x
  | x == y = x
  | x > y = error "uniform: requires not y < x"
  | isInfinite x && isInfinite y = inf -- TODO: coin flip for sign
  | isInfinite x = -inf
  | isInfinite y = inf
  | y <= 0 = - (uniformRightPositive (- y) (- x))
  | otherwise = uniformRightPositive x y

-- | Assumes x < y and y > 0.
uniformRightPositive ::
  (IEEERepr f, Num (Significand f)) =>
  f ->
  f ->
  f
uniformRightPositive x y
  | x < 0 = uniformSpansZero x y
  | expt x == expt y = uniformExponentsEqual (expt x) (sfcd x) (sfcd y)
  | sfcd x == 0 && sfcd y == 0 = uniformSignificandsZero (expt x) (expt y)
  | x == 0 = uniformLeftZero y
  | x == - y = uniformLeftZero y -- TODO: flip coin for sign
  | otherwise = uniformPositive x y

main :: IO ()
main = putStrLn "Hello, Haskell!"
