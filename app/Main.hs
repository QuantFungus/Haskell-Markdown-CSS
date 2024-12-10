module Main where

import Proj (processMarkdownFile)

main :: IO ()
main = do
  putStrLn "Enter the input Markdown file path:"
  inputFile <- getLine
  putStrLn "Enter the output HTML file path:"
  outputFile <- getLine
  processMarkdownFile inputFile outputFile