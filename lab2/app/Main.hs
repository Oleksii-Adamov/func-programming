module Main where
import Control.Parallel.Strategies

-- Function to integrate (example: x^2)
f :: Double -> Double
f x = x * x

parallelIntegral :: (Double -> Double) -> Double -> Double -> Int -> Double
parallelIntegral f a b n = sum parts * delta
  where
    delta = (b - a) / fromIntegral n
    xs = take n [a, a + delta ..]
    parts = runEval $ parList rseq (map f xs)

main :: IO ()
main = do
  let
    a = 0
    b = 10
    n = 1000000
  putStrLn $ "Integral value: " ++ show (parallelIntegral f a b n)
