module Commands (importImages) where

import Application
import Import hiding (empty, head)

import Data.ByteString (append, empty, head)
import Data.ByteString.Builder
import Data.Text.Encoding
import Data.Time.Format
import System.Directory
import System.FilePath

import VideoDownload.ImportFrame (importFrame)

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
    case maybeTime of
      Just time ->
        importFrame filePath directionId time
      Nothing ->
        liftIO $ putStrLn ("Cannot read time from file name " ++ (fromString fileName))
