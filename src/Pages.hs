{-# LANGUAGE OverloadedStrings #-}

module Pages where

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

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (methodPut)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS

putPage :: String -> Text -> Text -> ReaderT Configuration IO ()
putPage url body title = do
  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- parseRequest url

  let jsonBody = encode $ object [ "wiki_page" .= object [ "body" .= body, "title" .= title ] ]

  theToken <- asks token

  let request = initialRequest
                { method = methodPut
                , requestBody = RequestBodyLBS jsonBody
                , requestHeaders = [("Authorization", BS.pack ("Bearer " ++ (unpack theToken))),
                                     ("Content-Type", "application/json")]
                }

  response <- liftIO $ httpLbs request manager

  pure ()

showPage :: Page -> Text
showPage (Page { pageUrl = (Just (NonNull url)) }) = url

listPages :: ReaderT Configuration IO ()
listPages = do
  course <- asks courseId
  config <- asks canvasConfig
  pages <- liftIO $ Canvas.runWithConfiguration config $ do
    Canvas.listPagesCourses $ Canvas.mkListPagesCoursesParameters course

  case (responseBody pages) of
    Canvas.ListPagesCoursesResponse200 pages -> mapM_ (liftIO . putStrLn . unpack . showPage) pages
    Canvas.ListPagesCoursesResponseError e -> error e

uploadTex :: FilePath -> ReaderT Configuration IO ()
uploadTex path = do
  let readerOptions = def 
  latex <- liftIO $ TIO.readFile path
  pandoc <- liftIO $ runIOorExplode $ do
    readLaTeX readerOptions latex

  let Pandoc meta _ = pandoc

  let title = case lookupMeta "title" meta of
                Just (MetaString t)   -> t
                Just (MetaInlines is) -> stringify is
                _                     -> pack path

  let writerOptions = def
                      { writerHTMLMathMethod = MathJax ""
                      , writerExtensions = pandocExtensions
                      }

  result <- liftIO $ runIOorExplode $ writeHtml5String writerOptions pandoc

  course <- asks courseId
  config <- asks canvasConfig
  let url = pack path

  let params = mkUpdateCreatePageCoursesParameters course url

  pages <- liftIO $ runWithConfiguration config $ do
    updateCreatePageCourses params Nothing

  rootUrl <- asks apiBase
  putPage ((unpack rootUrl) ++ "/v1/courses/" ++ (unpack course) ++ "/pages/" ++ (unpack url)) result title

  pure ()

uploadMarkdown :: FilePath -> ReaderT Configuration IO ()
uploadMarkdown path = do
  error "Cannot currently process markdown files"

uploadPage :: FilePath -> ReaderT Configuration IO ()
uploadPage path = do
    case takeExtension path of
        ".tex" -> uploadTex path
        ".md"  -> uploadMarkdown path
        _      -> error $ "Unknown file extension: " ++ path

uploadPages :: [FilePath] -> ReaderT Configuration IO ()
uploadPages paths = do
  mapM_ uploadPage paths

