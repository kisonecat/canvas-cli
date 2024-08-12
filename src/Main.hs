{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory ( listDirectory, doesFileExist )
import System.Process (callProcess)
import Network.HTTP.Simple
import System.FilePath (replaceExtension)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (isSuffixOf, pack)
import Data.Text.Encoding
import qualified Canvas

import qualified Configuration.Dotenv
import qualified System.Environment (lookupEnv)
import Control.Monad

import Options.Applicative

import Control.Monad.Reader
import Configuration

import Pages
import Files

data Command
  = Files Subcommand
  | Pages Subcommand
  deriving Show

data Subcommand
  = Upload [FilePath]
  | List
  deriving Show

sample :: Parser Command
sample = subparser
          ( command "files" (info (filesCommand <**> helper) ( progDesc "Upload or list files" ))
            <> command "pages" (info (pagesCommand <**> helper) ( progDesc "Upload or list pages" ))
          )

subcommand :: String -> Parser Subcommand
subcommand thing = subparser ((command "upload" (info (Upload <$> some (fileArgument thing)) (progDesc ("upload " ++ thing))))
                                <> (command "list" (info (pure $ List)  (progDesc ("list existing " ++ thing))))
                               )

filesCommand :: Parser Command
filesCommand = Files <$> subcommand "files"

pagesCommand :: Parser Command
pagesCommand = Pages <$> subcommand "pages"

fileArgument :: String -> Parser FilePath
fileArgument thing = strArgument
  ( metavar "FILES..."
    <> help ("List of " ++ thing ++ " to upload")
  )

main :: IO ()
main = do
  fileExists <- doesFileExist ".env"
  when fileExists $ Configuration.Dotenv.loadFile Configuration.Dotenv.defaultConfig

  cCourseId' <- System.Environment.lookupEnv "COURSE_ID"
  cCourseId <- maybe (error "Missing $COURSE_ID") (pure . T.pack) cCourseId'

  theToken' <- System.Environment.lookupEnv "ACCESS_TOKEN"
  theToken <- maybe (error "Missing $ACCESS_TOKEN") (pure . T.pack) theToken'

  baseUrl' <- System.Environment.lookupEnv "BASE_URL"
  baseUrl <- maybe (error "Missing $BASE_URL") (pure . T.pack) baseUrl'

  let securityScheme = Canvas.bearerAuthenticationSecurityScheme theToken
  let config = Configuration {
        token = theToken,
        apiBase = baseUrl,
        canvasConfig = Canvas.defaultConfiguration { Canvas.configBaseURL = baseUrl
                                                   , Canvas.configSecurityScheme = securityScheme
                                                   },
        courseId = cCourseId
        }

  command <- execParser $ info (sample <**> helper)
             ( fullDesc
               <> progDesc "Interact with the Canvas API"
               <> header "canvas - a CLI tool for Canvas" )

  case command of
    Files (Upload files) -> do
      runReaderT (uploadFiles files) config
    Files List -> do
      runReaderT listFiles config
    Pages (Upload files) -> do
      runReaderT (uploadPages files) config
    Pages List -> do
      runReaderT listPages config

