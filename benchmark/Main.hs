{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Relude

import Data.IntMap (IntMap)
import Data.Vebt (VEBT)
import Gauge.Main (Benchmark, bench, defaultMain, whnf)

import qualified Data.IntMap as IM
import qualified Data.Vebt as VEB

instance NFData v => NFData (VEBT v)

main :: IO ()
main = do
    let intMapEntries = zip [0..1024] [0..]
    let vebMapEntries = zip [0..1024] ([0..] :: [Int])

    let intMap = IM.fromAscList intMapEntries
    let vebMap = flipfoldl' (uncurry VEB.insert) VEB.empty vebMapEntries

    evaluateNF_ intMap
    evaluateNF_ vebMap

    defaultMain
        [ intMapBench "int/all" intMap (map fst intMapEntries)
        , vebMapBench "veb/all" vebMap (map fst vebMapEntries)
        ]

intMapBench :: String -> IntMap Int -> [Int] -> Benchmark
intMapBench label m = bench label . whnf (go 0)
  where
    go :: Int -> [Int] -> Int
    go !acc [] = acc
    go !acc (x:xs) = case IM.lookup x m of
        Nothing -> go acc xs
        Just y  -> go (acc + y) xs

vebMapBench :: String -> VEBT Int -> [Word64] -> Benchmark
vebMapBench label m = bench label . whnf (go 0)
  where
    go :: Int -> [Word64] -> Int
    go !acc [] = acc
    go !acc (x:xs) = case VEB.lookup x m of
        Nothing -> go acc xs
        Just y  -> go (acc + y) xs
