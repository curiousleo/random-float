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

  expt :: f -> Exponent f
  sfcd :: f -> Significand f

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

uniform ::
  IEEERepr f =>
  f ->
  f ->
  f
uniform x y
  | isNaN x || isNaN y = x
  | x == y = x
  | x > y = error "uniform: requires not y < x"
  | isInfinite x && isInfinite y = x -- TODO: coin flip for sign x or y
  | isInfinite x = x
  | isInfinite y = y
  | y <= 0 = - (uniformRightPositive (- y) (- x))
  | otherwise = uniformRightPositive x y

-- | Assumes x < y and y > 0.
uniformRightPositive ::
  IEEERepr f =>
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
