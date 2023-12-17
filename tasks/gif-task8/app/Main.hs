import Codec.Picture
import Codec.Picture.Gif
import Codec.Picture.Types
import Control.Monad
import qualified Data.Vector.Storable as V

-- Function to hide a bit in a number
hideBit :: Pixel8 -> Bool -> Pixel8
hideBit byte bitToHide =
  if bitToHide
    then byte `div` 2 * 2 + 1 -- make the last bit 1
    else byte `div` 2 * 2     -- make the last bit 0

-- Function to hide text in a GIF file
hideTextInGIF :: String -> FilePath -> FilePath -> IO ()
hideTextInGIF text inputPath outputPath = do
  dynamicImage <- readGif inputPath
  case dynamicImage of
    Left err -> putStrLn $ "Erroe reading file: " ++ err
    Right (ImageRGB8 img) -> do
      let textBytes = map fromIntegral $ map fromEnum text
          pixelData = imageData img
          pixelDataWithHiddenText = V.zipWith hideTextByte pixelData (cycle textBytes)
          imgWithHiddenText = ImageRGB8 $ img {imageData = pixelDataWithHiddenText}
      saveGifImage outputPath (ImageRGB8 imgWithHiddenText)
    Right _ -> putStrLn "This program supports only RGB8 format images in GI"

  where
    hideTextByte :: PixelRGB8 -> Pixel8 -> PixelRGB8
    hideTextByte (PixelRGB8 r g b) byte =
      let [r', g', b'] = map (hideBit) [r, g, b]
      in PixelRGB8 r' g' b'

-- Function to get hidden text from a GIF file
getHiddenTextFromGIF :: FilePath -> IO (Maybe String)
getHiddenTextFromGIF filePath = do
  dynamicImage <- readGif filePath
  case dynamicImage of
    Left err -> do
      putStrLn $ "Erroe reading file: " ++ err
      return Nothing
    Right (ImageRGB8 img) -> do
      let pixelData = imageData img
          hiddenBytes = extractHiddenText pixelData []
      return $ Just (map (toEnum . fromIntegral) hiddenBytes)
    Right _ -> do
      putStrLn "This program supports only RGB8 format images in GI"
      return Nothing

  where
    extractHiddenText :: V.Vector PixelRGB8 -> [Pixel8] -> [Pixel8]
    extractHiddenText pixels acc
      | V.null pixels = reverse acc
      | otherwise = let (bytes, rest) = V.splitAt 3 pixels
                        [r, g, b] = V.toList $ V.map extractHiddenBit $ V.take 3 pixels
                    in extractHiddenText rest (r : g : b : acc)

    extractHiddenBit :: PixelRGB8 -> Pixel8
    extractHiddenBit (PixelRGB8 r g b) =
      let rBit = r .&. 1
          gBit = g .&. 1
          bBit = b .&. 1
      in rBit * 4 + gBit * 2 + bBit

main :: IO ()
main = do
  let inputFilePath = "lena_color.gif"
      outputFilePath = "output.gif"
      textToHide = "Hidden text"
  hideTextInGIF textToHide inputFilePath outputFilePath
  maybeText <- getHiddenTextFromGIF outputFilePath
    case maybeText of
      Just text -> putStrLn $ "Extracted Hidden Text: " ++ text
      Nothing -> putStrLn "Failed to extract hidden text."