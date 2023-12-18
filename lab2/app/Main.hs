module Main where
import Control.Parallel.Strategies
import System.CPUTime
import Text.Printf

-- Function to integrate (example: x^2)
f :: Double -> Double
f x = x * x

-- Calculate the integral using a parallel approach with required precision (eps)
parallelIntegral :: (Double -> Double) -> Double -> Double -> Double -> Double
parallelIntegral f a b eps = integralLoop 1 Nothing
  where
    integralLoop n prevIntegral =
      let
        delta = (b - a) / fromIntegral n
        xs = take n [a, a + delta ..]
        parts = runEval $ parList rseq (map f xs)
        integral = sum parts * delta
      in
        case prevIntegral of
          Nothing -> integralLoop (n * 2) (Just integral)
          Just prev ->
            if abs (integral - prev) < eps
              then integral
              else integralLoop (n * 2) (Just integral)

-- Calculate the integral using a sequential approach with required precision (eps)
sequentialIntegral :: (Double -> Double) -> Double -> Double -> Double -> Double
sequentialIntegral f a b eps = integralLoop 1 Nothing
  where
    integralLoop n prevIntegral =
      let
        delta = (b - a) / fromIntegral n
        xs = take n [a, a + delta ..]
        parts = map f xs
        integral = sum parts * delta
      in
        case prevIntegral of
          Nothing -> integralLoop (n * 2) (Just integral)
          Just prev ->
            if abs (integral - prev) < eps
              then integral
              else integralLoop (n * 2) (Just integral)

-- Measure execution time for a given integral calculation function
measureExecutionTime :: ((Double -> Double) -> Double -> Double -> Double -> Double) -> Double -> Double -> Double -> IO Double
measureExecutionTime calcFunction a b eps = do
  startTime <- getCPUTime
  let result = calcFunction f a b eps
  endTime <- result `seq` getCPUTime
  let elapsedTime = fromIntegral (endTime - startTime) / (10^12)
  printf "Time: %.9f seconds\n" (elapsedTime :: Double)
  return result

main :: IO ()
main = do
  let
    a = 0
    b = 10
    eps = 0.001
  putStrLn "Parallel algorithm:"
  parallelResult <- measureExecutionTime parallelIntegral a b eps
  putStrLn "Sequential algorithm:"
  sequentialResult <- measureExecutionTime sequentialIntegral a b eps
  putStrLn $ "Parallel integral value: " ++ show parallelResult
  putStrLn $ "Sequential integral value: " ++ show sequentialResult
