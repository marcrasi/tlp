module VideoDownload.ImportFrame (importFrame) where

import Import hiding (empty)

import Codec.Picture
import Codec.Picture.Types
import Crypto.Hash.SHA1 as SHA1
import Data.ByteString (append, empty)
import Data.ByteString.Builder
import Data.Text.Encoding
import Data.Time.Format
import Data.Vector.Storable.ByteString
import Vision.Image.JuicyPixels
import Vision.Image.RGB.Type
import Vision.Image.Type

import Data.Conduit
import qualified Data.Conduit.Binary as CB

importFrame :: FilePath -> DirectionId -> UTCTime -> Handler ()
importFrame filePath directionId time = do
    imageByteString <- liftIO $ runResourceT $ CB.sourceFile filePath $$ sinkByteString
    let imageEither = decodeImageWithMetadata imageByteString
    case imageEither of
      Left error ->
        liftIO $ print error
      Right (ImageYCbCr8 image, _) ->
        processImage filePath directionId time (toFridayRGB $ convertImage image)
      _ ->
        liftIO $ putStrLn "Unsupported image type."

processImage :: FilePath -> DirectionId -> UTCTime -> RGB -> Handler ()
processImage filePath directionId time image = do
  let hash = hashRGB image
  let frame = Frame { frameHash = hash, frameFilename = (fromString filePath), frameCapturedAt = time, frameDirection = directionId }
  withSameHash <- runDB $ getBy (UniqueHash hash)
  case withSameHash of
    Just _ ->
      liftIO $ putStrLn ("Image " ++ (fromString filePath) ++ " already imported. Skipping.")
    Nothing -> do
      runDB $ insert frame
      return ()

hashRGB :: RGB -> Text
hashRGB rgb =
    decodeASCII $ toStrict $ toLazyByteString $ byteStringHex $ SHA1.hash $ vectorToByteString $ manifestVector rgb

sinkByteString :: (MonadResource m) => Sink ByteString m ByteString
sinkByteString = do
    mx <- await
    case mx of
      Just x -> do
        rest <- sinkByteString
        return $ append x rest
      Nothing ->
        return empty

