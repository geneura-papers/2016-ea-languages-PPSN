{-
bitflip.hs
Description: Random bitflips over a random boolean vector
License: GPL-3

This code creates a random boolean vector and applies a fixed number of random
bitflips over it. The time elapsed during the generation of random positions
and the application of the bitflips is measured.
-}

--
-- This code uses the Data.Sequence data structure to represent the boolean vectors,
-- which is implemented using Finger Trees, a purely functional data structure
-- supporting access to the ends in amortized constant time and concatenation and
-- splitting in time logarithmic in the size of the smaller piece.
-- 
-- More information can be found in the following article:
--    Ralf Hinze and Ross Paterson,
--    "Finger trees: a simple general-purpose data structure",
--    Journal of Functional Programming 16:2 (2006) pp 197-217.
--    http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
--
import Data.Sequence hiding (take)
import Data.Time
import Control.DeepSeq
import System.Random

-- | Number of applications of the bitflip function
iterations :: Int
iterations = 100000

-- | The bitflip function
bitflip :: Seq Bool -- ^ Original vector
        -> Int      -- ^ Position where to apply the bitflip
        -> Seq Bool
bitflip v n = update n (not (index v n)) v


-- | Complete benchmark over a vector of a given size. It generates a random
-- vector, starts timing, generates a random vector of positions and applies
-- a bitflip over those positions.
benchmark :: Int   -- ^ Lenght of the benchmark vector
          -> IO ()
benchmark n = do
  -- Random vector generation
  gen <- newStdGen
  let rbools = randoms gen :: [Bool]
  let vector = fromList $ take n rbools

  -- Starts measuring the time
  start <- getCurrentTime

  -- Random bitflips
  let rflips = take iterations $ randomRs (0,n-1) gen :: [Int]
  let changedvector = foldl bitflip vector rflips

  -- Stops measuring the time.
  -- Here 'deepseq' is neccessary to prevent lazy evaluation from giving us
  -- an incorrect measure.
  stop <- (changedvector `deepseq` getCurrentTime)
  let diffTime = diffUTCTime stop start

  -- Printing
  putStrLn $ concat ["Haskell-Sequence, ", show n, ", ", show diffTime]

main :: IO ()
main = do
  sequence_ $ map benchmark ((2^) <$> [4..16])
  return ()
