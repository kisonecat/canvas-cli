{-# LANGUAGE OverloadedStrings #-}

module Files where

import Control.Monad.Reader
import Configuration
import Canvas hiding (Course(..), Configuration(..))
import Network.HTTP.Client
import Data.Text
import System.FilePath (takeExtension)
import qualified Data.Text.IO as TIO

import Text.Pandoc
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options
import Text.Pandoc.Shared

import qualified Data.ByteString.Lazy as B

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Client.MultipartFormData
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe

showFile :: File -> Text
showFile (File { fileDisplayName = (Just (NonNull url)) }) = url

listFiles :: ReaderT Configuration IO ()
listFiles = do
  course <- asks courseId
  config <- asks canvasConfig
  files <- liftIO $ Canvas.runWithConfiguration config $ do
    Canvas.listFilesCourses $ Canvas.mkListFilesCoursesParameters course

  case (responseBody files) of
    Canvas.ListFilesCoursesResponse200 files -> mapM_ (liftIO . putStrLn . unpack . showFile) files
    Canvas.ListFilesCoursesResponseError e -> error e

extractUploadURL :: Object -> Text
extractUploadURL obj = fromJust $ parseMaybe (.: "upload_url") obj

postFile :: String -> String -> FilePath -> ReaderT Configuration IO ()
postFile url name path = do
  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- parseRequest url

  let jsonBody = encode $ object [ "name" .= name, "parent_folder_path" .= ("/" :: Text) ]

  theToken <- asks token

  let request = initialRequest
                { method = methodPost
                , requestBody = RequestBodyLBS jsonBody
                , requestHeaders = [("Authorization", BS.pack ("Bearer " ++ (unpack theToken))),
                                     ("Content-Type", "application/json")]
                }

  response <- liftIO $ httpLbs request manager

  let (Just result) = decode (responseBody response) :: Maybe Object

  initialRequest <- (parseRequest $ unpack $ extractUploadURL result)
  
  let initialRequest' = initialRequest { method = methodPost
                                       }

  formRequest <- formDataBody
                 [ partFileSource "file" path
                 ] initialRequest'

  response <- liftIO $ httpLbs formRequest manager

  pure ()


uploadFile :: FilePath -> ReaderT Configuration IO ()
uploadFile path = do
  content <- liftIO $ B.readFile path

  course <- asks courseId
  config <- asks canvasConfig
  let url = pack path

  rootUrl <- asks apiBase
  postFile ((unpack rootUrl) ++ "/v1/courses/" ++ (unpack course) ++ "/files") path path
  
  pure ()

uploadFiles :: [FilePath] -> ReaderT Configuration IO ()
uploadFiles paths = do
  mapM_ uploadFile paths

