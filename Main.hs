{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory)
import System.Process (callProcess)
import Network.HTTP.Simple
import System.FilePath (replaceExtension)
import Text.Pandoc
import Text.Pandoc.Error (PandocError)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (isSuffixOf, pack)
import Data.Text.Encoding
import Canvas
import Network.HTTP.Client

import qualified Configuration.Dotenv
import qualified System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Control.Monad

main :: IO ()
main = do
  fileExists <- doesFileExist ".env"
  when fileExists $ Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  cCourseId' <- System.Environment.lookupEnv "COURSE_ID"
  cCourseId <- maybe (error "Missing $COURSE_ID") (pure . T.pack) cCourseId'

  token' <- System.Environment.lookupEnv "ACCESS_TOKEN"
  token <- maybe (error "Missing $ACCESS_TOKEN") (pure . T.pack) token'

  baseUrl' <- System.Environment.lookupEnv "BASE_URL"
  baseUrl <- maybe (error "Missing $BASE_URL") (pure . T.pack) baseUrl'

  let cAssignmentId = "123"

  let securityScheme = bearerAuthenticationSecurityScheme token
  let canvasConfig = defaultConfiguration { configBaseURL = baseUrl
                                          , configSecurityScheme = securityScheme
                                          }

  pages <- 
    runWithConfiguration canvasConfig $ do
      listPagesCourses $ mkListPagesCoursesParameters cCourseId

  putStrLn $ show $ responseBody pages


main2 :: IO ()
main2 = do
    -- List all files in the current directory
    files <- listDirectory "."

    -- Convert file names to Text
    let textFiles = map T.pack files

    -- Filter out .tex files
    let texFiles = filter (".tex" `isSuffixOf`) textFiles

    -- Run pdflatex on each .tex file
    mapM_ (callProcess "pdflatex" . (:[]) . T.unpack) texFiles

    -- Convert .tex files to .html using pandoc
    mapM_ (convertToHtml . T.unpack) texFiles

    -- Upload each resulting .pdf file
    --let pdfFiles = map (T.unpack . (`replaceExtension` ".pdf")) texFiles
    --mapM_ uploadFile pdfFiles

    -- Upload each resulting .html file
    --let htmlFiles = map (T.unpack . (`replaceExtension` ".html")) texFiles
    --mapM_ uploadFile htmlFiles
    pure ()

convertToHtml :: FilePath -> IO ()
convertToHtml texFile = do
    let htmlFile = replaceExtension texFile ".html"
    let readerOptions = def -- Default options for the reader
    latex <- TIO.readFile texFile
    let result = runPure $ do
                 readLaTeX readerOptions latex
    -- writeHtml5String def doc
    -- writeFile htmlFile res
    pure ()

uploadFile :: FilePath -> IO ()
uploadFile file = do
    let request = setRequestMethod "PUT"
                $ setRequestPath ("/upload/" `mappend` encodeUtf8 (pack file))
                $ setRequestHost "example.com"
                $ setRequestPort 80
                $ setRequestBodyFile file
                $ defaultRequest
    response <- httpLBS request
    print $ getResponseStatusCode response
