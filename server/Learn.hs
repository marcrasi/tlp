module Learn where

import Application
import Import

import Codec.Picture
import Codec.Picture.Types
import Database.Persist.Sql (toSqlKey)
import System.IO (withBinaryFile, IOMode(ReadMode))
import qualified Vision.Image as I
import Vision.Image.Conversion
import Vision.Image.Filter
import Vision.Image.Grey.Type
import Vision.Image.JuicyPixels
import Vision.Image.RGB.Type
import Vision.Image.Type


learn :: IO ()
learn = handler learnHandler 

learnHandler :: Handler ()
learnHandler = do
    frame <- runDB $ get404 (toSqlKey 4520 :: FrameId)
    imageEither <- liftIO $ withBinaryFile (unpack $ frameFilename frame) ReadMode (\x -> fmap decodeImage (hGetContents x))
    image <- case imageEither of
      Left error -> invalidArgs ["Could not load image."]
      Right (ImageYCbCr8 image) -> return $ toFridayRGB $ convertImage image
      Right _ -> invalidArgs ["Unknown image type."]
    let blurred = colorGaussianBlur 20 image 
    let rejuiced = ImageRGB8 $ toJuicyRGB blurred 
    liftIO $ savePngImage "example.png" rejuiced
    return ()

colorGaussianBlur :: Int -> RGB -> RGB
colorGaussianBlur radius image =
    let
      blurF = gaussianBlur radius (Nothing :: Maybe Double)
      red = blurF $ redChannel image
      green = blurF $ greenChannel image
      blue = blurF $ blueChannel image
    in
      combineChannels red green blue

redChannel :: RGB -> Grey
redChannel = I.map (GreyPixel . rgbRed)

greenChannel :: RGB -> Grey
greenChannel = I.map (GreyPixel . rgbGreen)

blueChannel :: RGB -> Grey
blueChannel = I.map (GreyPixel . rgbBlue)

combineChannels :: Grey -> Grey -> Grey -> RGB
combineChannels red green blue =
    I.fromFunction (I.shape red) $ \pt ->
      RGBPixel (pixelValue $ I.index red pt) (pixelValue $ I.index green pt) (pixelValue $ I.index blue pt)

pixelValue :: GreyPixel -> Word8
pixelValue (GreyPixel v) = v
