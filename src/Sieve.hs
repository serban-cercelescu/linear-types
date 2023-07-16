module Sieve (sieve) where

import qualified Data.Array.Mutable.Linear as Array
import qualified Prelude as P
import Data.Array.Mutable.Linear (Array)
import Data.Unrestricted.Linear
import Prelude.Linear



nums :: Int -> [Int]
nums n = unur $ Array.alloc n 0 $ \arr ->
    Array.toList $ Array.set 0 n arr


sieve :: Int -> [Int]
sieve n = ans where
    ans = P.map P.snd $
          P.filter P.fst $
          P.zip boolList [0..]

    (Ur boolList) = Array.alloc n True $ \arr -> Array.toList $
        Array.set 0 False arr & \arr ->
        Array.set 1 False arr & \arr ->
        go arr 2

go :: Array Bool %1 -> Int -> Array Bool
go arr k = Array.size arr & \(Ur n, arr) -> if k >= n then arr else
    Array.read arr k & \(Ur b, arr) -> b & \case {
        False -> go arr (k+1);
        True -> go (myFilter arr k (k*2) n) (k+1);
    }
    where
        myFilter :: Array Bool %1 -> Int -> Int -> Int -> Array Bool
        myFilter arr k i n = if i >= n then arr else
            Array.set i False arr & \arr ->
            myFilter arr k (i+k) n
