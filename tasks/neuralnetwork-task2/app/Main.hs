import System.Random
import Control.Monad
import Data.List (transpose)

type Layer = [[Double]]
type Network = [Layer]

-- Initialization of weight layers in the network
initializeWeights :: Int -> Int -> IO Layer
initializeWeights n m = replicateM n (replicateM m randomIO)

-- Activation function (sigmoid)
sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

-- Derivative of the activation function
sigmoid' :: Double -> Double
sigmoid' x = sigmoid x * (1 - sigmoid x)

-- Calculating the output signal of a layer using the activation function
feedForward :: Layer -> [Double] -> [Double]
feedForward w x = map sigmoid [sum $ zipWith (*) neuron x | neuron <- w]
  where
    neuron = map head w

-- Updating weights of network layers using gradient descent
updateWeights :: Double -> Layer -> Layer -> [Double] -> [Double] -> Layer
updateWeights learningRate w dw input output =
  zipWith (\weights delta -> zipWith (\weight d -> weight - learningRate * d) weights delta) w dw
  where
    delta = zipWith (*) (map (learningRate *) output) input

-- Backpropagation for a single training example
backpropagation :: Network -> [Double] -> [Double] -> Double -> Network
backpropagation network input target learningRate =
  let
    [hiddenLayer, outputLayer] = network

    hiddenOutput = feedForward hiddenLayer input
    finalOutput = feedForward outputLayer hiddenOutput

    outputError = zipWith (-) target finalOutput
    outputDelta = zipWith (*) outputError (map sigmoid' finalOutput)

    hiddenError = map sum $ transpose [zipWith (*) outputDelta neuron | neuron <- outputLayer]
    hiddenDelta = zipWith (*) hiddenError (map sigmoid' hiddenOutput)

    updatedOutputLayer = updateWeights learningRate outputLayer [outputDelta, outputDelta, outputDelta] hiddenOutput finalOutput
    updatedHiddenLayer = updateWeights learningRate hiddenLayer [hiddenDelta, hiddenDelta, hiddenDelta] input hiddenOutput
  in
    [updatedHiddenLayer, updatedOutputLayer]

-- Training the network
trainNetwork :: Network -> [([Double], [Double])] -> Int -> Double -> IO Network
trainNetwork network dataset epochs learningRate =
  foldM (\net _ ->
    foldM (\acc (input, target) ->
      return $ backpropagation acc input target learningRate
    ) net dataset
  ) network (replicate epochs ())

-- Function for predicting colors
predictColor :: Network -> [Double] -> [Double]
predictColor network input =
  let
    [hiddenLayer, outputLayer] = network
    hiddenOutput = feedForward hiddenLayer input
    finalOutput = feedForward outputLayer hiddenOutput
  in
    finalOutput

main :: IO ()
main = do
  let inputSize = 3 -- 3 inputs for RGB color values
      hiddenSize = 5
      outputSize = 3 -- 3 outputs for color classification (e.g., red, green, blue)
      learningRate = 0.1
      epochs = 10

  initialHiddenWeights <- initializeWeights hiddenSize inputSize
  initialOutputWeights <- initializeWeights outputSize hiddenSize

  let initialNetwork = [initialHiddenWeights, initialOutputWeights]

  -- Dataset for training (color examples in RGB format)
  let dataset = [ ([1, 0, 0], [1, 0, 0])  -- red
                , ([0, 1, 0], [0, 1, 0])  -- green
                , ([0, 0, 1], [0, 0, 1])  -- blue
                ]

  trainedNetwork <- trainNetwork initialNetwork dataset epochs learningRate

  -- Predicting a color for a new RGB value
  let inputColor = [0.5, 0.5, 0.0] -- new color
      predictedColor = predictColor trainedNetwork inputColor
  putStrLn $ "Predicted color for RGB values (0.5, 0.5, 0.0):"
  print predictedColor