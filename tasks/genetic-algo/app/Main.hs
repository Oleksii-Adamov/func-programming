module Main where

import System.Random
import Data.List
import Control.Monad (replicateM)

-- Define a data structure to represent a city with its (x, y) coordinates
data City = City { cityId :: Int, xCoord :: Double, yCoord :: Double }
  deriving (Eq, Show)

-- Define a data structure to represent a tour (a list of cities)
type Tour = [City]

-- Function to calculate the total distance of a tour
tourDistance :: Tour -> Double
tourDistance [] = 0
tourDistance [_] = 0
tourDistance (c1:c2:rest) =
  distance c1 c2 + tourDistance (c2:rest)

distance :: City -> City -> Double
distance city1 city2 =
  let dx = xCoord city1 - xCoord city2
      dy = yCoord city1 - yCoord city2
  in sqrt (dx * dx + dy * dy)

randomCities :: Int -> IO [City]
randomCities numCities = do
  cities <- replicateM numCities randomCity
  return cities

randomCity :: IO City
randomCity = do
  x <- randomRIO (0.0, 100.0)
  y <- randomRIO (0.0, 100.0)
  return $ City 0 x y

-- Function to generate a random tour
randomTour :: [City] -> IO Tour
randomTour cities = shuffle cities

-- Function to shuffle a list randomly
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (left, (x:right)) = splitAt i xs
  rest <- shuffle (left ++ right)
  return (x:rest)

-- Function to generate a population of random tours
generatePopulation :: Int -> [City] -> IO [Tour]
generatePopulation populationSize cities =
  sequence $ replicate populationSize (randomTour cities)

-- Function to calculate the fitness of a tour (lower distance is better)
fitness :: Tour -> Double
fitness tour = 1 / (tourDistance tour + 1)

-- Function to perform roulette wheel selection
rouletteSelect :: [Tour] -> IO Tour
rouletteSelect population = do
  let fitnessValues = map fitness population
      totalFitness = sum fitnessValues
  roulette <- randomRIO (0.0, totalFitness)
  return $ select roulette fitnessValues population

-- Helper function for roulette selection
select :: Double -> [Double] -> [Tour] -> Tour
select _ [] _ = error "Empty list of fitness values"
select r (fv:fvs) (t:ts)
  | r <= fv = t
  | otherwise = select (r - fv) fvs ts

-- Function to create the next generation using roulette selection
nextGeneration :: [City] -> Int -> [Tour] ->  IO [Tour]
nextGeneration cities numberOfElites population = do
  let populationSize = length population
  parents <- replicateM (populationSize - numberOfElites) (rouletteSelect population)
  children <- crossover cities parents
  let sortedPopulation = sortOn (negate . fitness) population
  let eliteTours = take numberOfElites sortedPopulation
  return (eliteTours ++ children)

-- Function to perform crossover
crossover :: [City] -> [Tour] -> IO [Tour]
crossover _ [] = return []
crossover _ [_] = return []
crossover cities (parent1:parent2:rest) = do
  crossoverPoint <- randomRIO (0, length cities - 1)
  let (segment1, _) = splitAt crossoverPoint parent1
      (segment2, _) = splitAt crossoverPoint parent2
      child1 = segment1 ++ (filter (`notElem` segment1) parent2)
      child2 = segment2 ++ (filter (`notElem` segment2) parent1)
  restChildren <- crossover cities rest
  return (child1 : child2 : restChildren)

mutate :: Tour -> IO Tour
mutate tour = do
  index1 <- randomRIO (0, length tour - 1)
  index2 <- randomRIO (0, length tour - 1)
  if index1 == index2
    then mutate tour
    else return $ swapCities index1 index2 tour

swapCities :: Int -> Int -> Tour -> Tour
swapCities i j tour
  | i == j = tour
  | i < j = swap i j tour
  | otherwise = swap j i tour

swap :: Int -> Int -> Tour -> Tour
swap i j tour =
  let cityI = tour !! i
      cityJ = tour !! j
      modifiedTour = take i tour ++ [cityJ] ++ drop (i + 1) tour
  in take j modifiedTour ++ [cityI] ++ drop (j + 1) modifiedTour

lengthsOfTours :: [Tour] -> [Int]
lengthsOfTours [] = []
lengthsOfTours (tour:rest) = (length tour) : (lengthsOfTours rest)

mutateWithChance:: Double -> Tour -> IO Tour
mutateWithChance mutationChance tour = do
  rand <- randomRIO (0.0, 1.0)
  if rand <= mutationChance
      then mutate tour
      else return tour

-- Function to evolve the population over a number of generations
evolve :: Int -> [City] -> [Tour] -> Double -> Int -> IO [Tour]
evolve 0 _ population _ _ = return population
evolve generations cities population mutationChance numberOfElites = do
  let mutatedPopulation = mapM  (mutateWithChance mutationChance) population
  newPopulation <- nextGeneration cities numberOfElites =<< mutatedPopulation
  let bestTour = minimumBy (\t1 t2 -> compare (tourDistance t1) (tourDistance t2)) newPopulation
  let bestDistance = tourDistance bestTour
  putStrLn $ "Generations left " ++ show (generations - 1) ++ " - Best distance: " ++ show bestDistance
  evolve (generations - 1) cities newPopulation mutationChance numberOfElites

-- Main function to run the genetic algorithm
main :: IO ()
main = do

  let populationSize = 500
  let numGenerations = 10000
--  let populationSize = 100
--  let numGenerations = 100
  let mutationChance = 0.013
  let numberOfElites = 2
  cities <- readCitiesFromFile "input51.txt"
  let numCities = length cities
  -- Create a list of random cities
--  let numCities = 10
--  cities <- randomCities numCities
  -- Generate an initial population of random tours
  initialPopulation <- generatePopulation populationSize cities
--  putStrLn $ show $ lengthsOfTours initialPopulation
  -- Run the genetic algorithm for a number of generations
  finalPopulation <- evolve numGenerations cities initialPopulation mutationChance numberOfElites

  -- Find the best tour in the final population
  let bestTour = minimumBy (\t1 t2 -> compare (tourDistance t1) (tourDistance t2)) finalPopulation
  let bestDistance = tourDistance bestTour

  putStrLn $ "Best tour distance: " ++ show bestDistance
  putStrLn $ "Best tour : " ++ show bestTour

-- Function to read cities' information from a file
readCitiesFromFile :: FilePath -> IO [City]
readCitiesFromFile filePath = do
  content <- readFile filePath
  let cityLines = lines content
  let cities = map (parseCity . words) cityLines
  return cities


-- Function to parse a line into a City
parseCity :: [String] -> City
parseCity [id, x, y] = City (read id) (read x) (read y)
parseCity _ = error "Invalid input format for city data."
