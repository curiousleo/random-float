{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module MonadIEEE where

import Data.Bool (bool)
import Data.Proxy (Proxy (Proxy))

-- TODO: instance IEEERepr, MonadIEEE

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
