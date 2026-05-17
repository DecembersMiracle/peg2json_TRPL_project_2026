module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import PEGParser
import JSONExporter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> processFile inputFile
        _ -> do
            putStrLn "Usage: peg-parser <input-file.peg>"
            exitFailure

processFile :: FilePath -> IO ()
processFile filePath = do
    input <- readFile filePath
    case parsePEG input of
        Left err -> do
            hPutStrLn stderr $ "Parse error: " ++ show err
            exitFailure
        Right ast -> do
            putStrLn (grammarToJSON ast)