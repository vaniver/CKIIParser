module Main where

import CKIIParser
import System.Environment ( getArgs)
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    inputFile <- T.readFile $ head args
    putStrLn $ show inputFile
    f inputFile