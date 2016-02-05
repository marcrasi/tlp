module Image.Loader where

import Import

import Codec.Picture (decodeImage)
import Codec.Picture.Types (convertImage, DynamicImage(ImageYCbCr8))
import System.IO (withBinaryFile, IOMode(ReadMode))
import Vision.Image.JuicyPixels (toFridayRGB)
import Vision.Image.RGB.Type (RGB)

import ExceptHandler

loadImage :: FilePath -> ExceptHandler RGB
loadImage path = do
    imageEither <- liftIO $ withBinaryFile path ReadMode (\x -> fmap decodeImage $ hGetContents x)
    case imageEither of
        Left error -> throwError $ pack $ show error
        Right (ImageYCbCr8 image) -> return $ toFridayRGB $ convertImage image
        Right _ -> throwError "Unknown Image Type"
