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
import qualified Canvas
import Network.HTTP.Client

import qualified Configuration.Dotenv
import qualified System.Environment (lookupEnv)
import System.Directory (doesFileExist)
import Control.Monad

import Options.Applicative

import Text.Pandoc
import Text.Pandoc.Options

import Control.Monad.Reader
import Configuration

data Command
  = Files Subcommand
  | Pages Subcommand
  deriving Show

data Subcommand
  = Upload
  | List
  deriving Show

sample :: Parser Command
sample = subparser
          ( (command "files" (info (filesCommand <**> helper) ( progDesc "Upload or list files" )))
            <> (command "pages" (info (pagesCommand <**> helper) ( progDesc "Upload or list pages" )))
          )

filesCommand :: Parser Command
filesCommand = subparser 
          ( (command "upload" (info (pure $ Files Upload) (progDesc "upload files" )))
            <> (command "list" (info (pure $ Files List)  (progDesc "list existing files" )))
          )

pagesCommand :: Parser Command
pagesCommand = subparser 
          ( (command "upload" (info (pure $ Pages Upload) ( progDesc "upload pages" )))
            <> (command "list" (info (pure $ Pages List)  ( progDesc "list existing pages" )))
          )

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

  let securityScheme = Canvas.bearerAuthenticationSecurityScheme token
  let config = Configuration {
        canvasConfig = Canvas.defaultConfiguration { Canvas.configBaseURL = baseUrl
                                                   , Canvas.configSecurityScheme = securityScheme
                                                   },
        courseId = cCourseId
        }
  
  command <- execParser $ info (sample <**> helper)
             ( fullDesc
               <> progDesc "Interact with the Canvas API"
               <> header "canvas - a CLI tool for Canvas" )

  putStrLn $ show command

  pure ()

-- | Converts a LaTeX document to HTML with MathJax for equations
convertLaTeXToHTML :: T.Text -> IO T.Text
convertLaTeXToHTML input = do
    -- Read the LaTeX input
    pandoc <- runIOorExplode $ readLaTeX def input

    -- Define options for the HTML writer
    let writerOptions = def 
            { writerHTMLMathMethod = MathJax ""
            , writerExtensions = pandocExtensions
            }

    -- Convert the Pandoc structure to HTML
    result <- runIOorExplode $ writeHtml5String writerOptions pandoc

    pure result

-- main2 :: IO ()
-- main2 = do
        
--   pages <- 
--     Canvas.runWithConfiguration canvasConfig $ do
--       listPagesCourses $ mkListPagesCoursesParameters cCourseId

--   putStrLn $ show $ responseBody pages



main3 :: IO ()
main3 = do
    -- List all files in the current directory
    files <- listDirectory "."

    -- Convert file names to Text
    let textFiles = map T.pack files

    -- Filter out .tex files
    let texFiles = filter (".tex" `isSuffixOf`) textFiles

    -- Run pdflatex on each .tex file
    --mapM_ (callProcess "pdflatex" . (:[]) . T.unpack) texFiles

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
    pandoc <- runIOorExplode $ do
                 readLaTeX readerOptions latex
    
    -- Define options for the HTML writer
    let writerOptions = def 
            { writerHTMLMathMethod = MathJax ""
            , writerExtensions = pandocExtensions
            }

    -- Convert the Pandoc structure to HTML
    result <- runIOorExplode $ writeHtml5String writerOptions pandoc
    
    print $ T.unpack result

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
