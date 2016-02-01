module VideoDownload.ImportFrame (importFrame) where

import Import hiding (empty)

import Codec.Picture
import Codec.Picture.Types
import Crypto.Hash.SHA1 as SHA1
import Data.ByteString (append, empty)
import Data.ByteString.Builder
import qualified Data.Text as DT
import Data.Text.Encoding
import Data.Time.Format
import Data.Vector.Storable.ByteString
import Vision.Image.JuicyPixels
import Vision.Image.RGB.Type
import Vision.Image.Type

import Data.Conduit
import qualified Data.Conduit.Binary as CB

importFrame :: FilePath -> DirectionId -> UTCTime -> Handler (Either Text (FrameId, Bool))
importFrame filePath directionId time = do
    imageByteString <- liftIO $ runResourceT $ CB.sourceFile filePath $$ sinkByteString
    let imageEither = decodeImageWithMetadata imageByteString
    case imageEither of
      Left error -> return $ Left $ DT.pack $ show error
      Right (ImageYCbCr8 image, _) -> do
        (frameId, isNew) <- processImage filePath directionId time (toFridayRGB $ convertImage image)
        return $ Right (frameId, isNew)
      _ -> return $ Left "Unsuppoerted image type."

processImage :: FilePath -> DirectionId -> UTCTime -> RGB -> Handler (FrameId, Bool)
processImage filePath directionId time image = do
  let hash = hashRGB image
  let frame = Frame { frameHash = hash, frameFilename = (fromString filePath), frameCapturedAt = time, frameDirection = directionId }
  withSameHash <- runDB $ getBy (UniqueHash hash)
  case withSameHash of
    Just (Entity frameId _) ->
      return (frameId, False)
    Nothing -> do
      frameId <- runDB $ insert frame
      return (frameId, True)

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

