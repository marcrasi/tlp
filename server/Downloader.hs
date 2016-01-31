import Application
import Import

getFrame :: String -> String -> String -> String -> IO ()
getFrame publicId intersection direction destination =
    handler $ getFrameHandler publicId intersection direction destination

getFrameHandler :: String -> String -> String -> String -> Handler ()
getFrameHandler publicId intersection direction destination = do
    request <- parseUrl "https://www.sccgov.org/sites/scc/_layouts/SCCGOV/SccgovProxy.aspx?a=tlj&b=%26ignore%3Dtrue"
    response <- withManager $ httpLbs request
    liftIO $ print $ responseBody response
