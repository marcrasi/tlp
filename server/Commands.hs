module Commands (importImages) where

import Application
import Import hiding (empty, head)

import Codec.Picture
import Codec.Picture.Types
import Crypto.Hash.SHA1 as SHA1
import Data.ByteString (append, empty, head)
import Data.ByteString.Builder
import Data.Text.Encoding
import Data.Time.Format
import Data.Vector.Storable.ByteString
import Vision.Image.JuicyPixels
import Vision.Image.RGB.Type
import Vision.Image.Type
import System.Directory
import System.FilePath

import Data.Conduit
import qualified Data.Conduit.Binary as CB

getChildren :: FilePath -> IO [FilePath]
getChildren src =
    getDirectoryContents src >>=
    return . filter (/= ".") >>=
    return . filter (/= "..")

getSubdirectories :: FilePath -> IO [FilePath]
getSubdirectories src =
    getChildren src >>=
    filterM (doesDirectoryExist . combine src)

getFiles :: FilePath -> IO [FilePath]
getFiles src =
    getChildren src >>=
    filterM (fmap not . doesDirectoryExist . combine src)

-- Import images from `src` directory into the database.
-- `src` must contain directories whose names are intersection names, and
-- each intersection directory must contain directories whose names are
-- direction names, and each direction directory must contain image files.
-- Image file names must be the unix timestamp for their capture time plus
-- an extension.
importImages :: FilePath -> IO ()
importImages = handler . importImagesHandler

importImagesHandler :: FilePath -> Handler ()
importImagesHandler src = do
    subdirectories <- liftIO $ getSubdirectories src
    mapM (processIntersection src) subdirectories
    return ()

processIntersection :: FilePath -> String -> Handler ()
processIntersection src name = do
    let directory = combine src name
    maybeEntity <- runDB $ getBy (UniqueIntersectionName (fromString name))
    case maybeEntity of
      Just (Entity id _) -> do
        subdirectories <- liftIO $ getSubdirectories directory
        mapM_ (processDirection directory id name) subdirectories
      Nothing ->
        liftIO $ putStrLn ("Not importing directory " ++ (fromString name) ++ " because it's not in the intersection table.")

processDirection :: FilePath -> IntersectionId -> String -> String -> Handler ()
processDirection src intersectionId intersectionName name = do
    let directory = combine src name
    maybeEntity <- runDB $ getBy (UniqueDirectionName intersectionId (fromString name))
    case maybeEntity of
      Just (Entity id _) -> do
        files <- liftIO $ getFiles directory
        mapM_ (processFile directory id) files 
      Nothing ->
        liftIO $ putStrLn ("Not importing directory " ++ (fromString name) ++ " from intersection " ++
          (fromString intersectionName) ++ " because it's not in the direction table.")

sinkByteString :: (MonadResource m) => Sink ByteString m ByteString
sinkByteString = do
    mx <- await
    case mx of
      Just x -> do
        rest <- sinkByteString
        return $ append x rest
      Nothing ->
        return empty

processFile :: FilePath -> DirectionId -> FilePath -> Handler ()
processFile src directionId fileName = do
    let filePath = combine src fileName
    let maybeTime = parseTimeM True defaultTimeLocale "%s" ((reverse . drop 3 . reverse . dropExtensions) fileName) :: Maybe UTCTime
    imageByteString <- liftIO $ runResourceT $ CB.sourceFile filePath $$ sinkByteString 
    let imageEither = decodeImageWithMetadata imageByteString
    case (imageEither, maybeTime) of
      (_, Nothing) ->
        liftIO $ putStrLn ("Cannot read time from file name " ++ (fromString fileName))
      (Left error, _) ->
        liftIO $ print error
      (Right (ImageYCbCr8 image, _), Just time) ->
        processImage filePath directionId time (toFridayRGB $ convertImage image)
      _ ->
        liftIO $ putStrLn "Unsupported image type."

processImage :: FilePath -> DirectionId -> UTCTime -> RGB -> Handler ()
processImage src directionId time image = do
  let hash = hashRGB image
  let frame = Frame { frameHash = hash, frameFilename = (fromString src), frameCapturedAt = time, frameDirection = directionId }
  withSameHash <- runDB $ getBy (UniqueHash hash)
  case withSameHash of
    Just _ ->
      liftIO $ putStrLn ("Image " ++ (fromString src) ++ " already imported. Skipping.")
    Nothing -> do
      runDB $ insert frame
      return ()

hashRGB :: RGB -> Text 
hashRGB rgb =
    decodeASCII $ toStrict $ toLazyByteString $ byteStringHex $ SHA1.hash $ vectorToByteString $ manifestVector rgb 
