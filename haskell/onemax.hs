{-
onemax.hs
Description: Computes the sum of the bits over a boolean vector
License: GPL-3

This code creates a random boolean vector and sums all of its bits.
The time elapsed between the start and getting the result of the sum
is measured.
-}

import Data.Sequence hiding (take)
import Data.Time
import Data.Foldable hiding (concat)
import Control.DeepSeq
import System.Random

-- | Sums the bits of a boolean vector
onemax :: Seq Bool -> Int
onemax v = Data.Foldable.foldl (\y -> (\x -> if x then y+1 else y)) 0 v

-- | Complete benchmark over a vector of a given size. It generates a random
-- vector, and counts the 1 bits in it.
benchmark :: Int -> IO ()
benchmark n = do
  -- Random vector generation
  gen <- newStdGen
  let rbools = randoms gen :: [Bool]
  let vector = fromList $ take n rbools

  -- Timing
  start <- getCurrentTime

  -- Counting
  let count = onemax vector

  -- Stops measuring the time.
  -- Here 'deepseq' is neccessary to prevent lazy evaluation from giving us
  -- an incorrect measure.  
  stop <- (count `deepseq` getCurrentTime)
  let diffTime = diffUTCTime stop start

  -- Printing
  putStrLn $ concat ["Haskell-Sequence, ", show n, ", ", show diffTime]

main :: IO ()
main = do
  sequence_ $ Prelude.map benchmark ((2^) <$> [4..16])
  return ()

