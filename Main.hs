{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (listDirectory)
import System.Process (callProcess)
import Network.HTTP.Simple
import System.FilePath (replaceExtension)
import Text.Pandoc

main :: IO ()
main = do
    -- List all files in the current directory
    files <- listDirectory "."

    -- Filter out .tex files
    let texFiles = filter (".tex" `isSuffixOf`) files

    -- Run pdflatex on each .tex file
    mapM_ (callProcess "pdflatex" . (:[])) texFiles

    -- Convert .tex files to .html using pandoc
    mapM_ convertToHtml texFiles

    -- Upload each resulting .pdf file
    let pdfFiles = map (`replaceExtension` ".pdf") texFiles
    mapM_ uploadFile pdfFiles

    -- Upload each resulting .html file
    let htmlFiles = map (`replaceExtension` ".html") texFiles
    mapM_ uploadFile htmlFiles

convertToHtml :: FilePath -> IO ()
convertToHtml texFile = do
    let htmlFile = replaceExtension texFile ".html"
    res <- runIOorExit $ do
        doc <- readLaTeX def =<< readFile texFile
        writeHtml5String def doc
    writeFile htmlFile res

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
