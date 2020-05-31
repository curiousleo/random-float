{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad.State.Strict (State, runState, state)
import Data.List (group, sort, unfoldr)
import Data.Proxy (Proxy (Proxy))
import GHC.Float (float2Double)
import MonadIEEE
import System.Random.SplitMix (SMGen, mkSMGen)
import Uniform
import qualified System.IO as IO
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Builder.Prim as BS
import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (getArgs)

main :: IO ()
main = do
  let !stdout = IO.stdout
  [m, t, x, y] <- getArgs
  ref <- newIORef (mkSMGen 13371)
  case t of
    "binary8" -> do
      let u = uniform (assemble (read x) :: Binary8, assemble (read y) :: Binary8)
      let output = case m of
                  "text" -> print
                  _ -> error "text only"
      forever $ do
        gen <- readIORef ref
        let (v, gen') = runState u gen
        output v
        writeIORef ref gen'
    "float" -> do
      let u = uniform (read x :: Float, read y :: Float)
      let output = case m of
                  "text" -> print
                  "binary" -> BS.hPutBuilder stdout . BS.primFixed BS.floatHost
                  _ -> error "text or binary"
      forever $ do
        gen <- readIORef ref
        let (v, gen') = runState u gen
        output v
        writeIORef ref gen'
    "double" -> do
      let u = uniform (read x :: Double, read y :: Double)
      let output = case m of
                  "text" -> print
                  "binary" -> BS.hPutBuilder stdout . BS.primFixed BS.doubleHost
                  _ -> error "text or binary"
      forever $ do
        gen <- readIORef ref
        let (v, gen') = runState u gen
        output v
        writeIORef ref gen'
    _ -> error "float or double"

  {-
  let x = assemble (6, 7) :: Binary8
  let y = assemble (7, 1) :: Binary8
  print x
  print y
  let n = 9000
  let fs = take n $ unfoldr (Just . runState (uniform (x, y))) (mkSMGen 1337)
  mapM_ (\xs -> putStrLn $ show (length xs) ++ "\t " ++ show (head xs)) (group $ sort fs)
  let toDouble = float2Double . toFloat
  let avg = sum (map toDouble fs) / (fromIntegral n :: Double)
  print avg
  print $ (toDouble x + toDouble y) / (2 :: Double)
  -}

-- Point out parsing issue: -3e-324 is parsed as -5e-324. There is nothing
-- the library can do to distinguish the two.
--
-- Also, averages may be skewed! Floats as real intervals.
