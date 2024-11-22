{-# LANGUAGE OverloadedStrings #-}

module MarkdownToHTML where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.IO

-- Data type to represent different Markdown elements
data MarkdownElement = Header Int String
                     | Paragraph String
                     | UnorderedList [String]
                     | Link String String
                     deriving (Show)

-- Parsers for different elements

-- Parse a header (e.g., # Header 1)
headerParser :: Parser MarkdownElement
headerParser = do
    hashes <- many1 (char '#')
    space
    content <- manyTill anyChar newline
    return $ Header (length hashes) content

-- Parse a paragraph
paragraphParser :: Parser MarkdownElement
paragraphParser = do
    content <- manyTill anyChar newline
    return $ Paragraph content

-- Parse an unordered list
unorderedListParser :: Parser MarkdownElement
unorderedListParser = do
    items <- many1 (char '-' >> space >> manyTill anyChar newline)
    return $ UnorderedList items

-- Parse a link (e.g., [Link](https://example.com))
linkParser :: Parser MarkdownElement
linkParser = do
    _ <- char '['
    text <- manyTill anyChar (char ']')
    _ <- char '('
    url <- manyTill anyChar (char ')')
    return $ Link text url

-- Combine all parsers into a single parser
markdownParser :: Parser [MarkdownElement]
markdownParser = many $ choice
    [ try headerParser
    , try unorderedListParser
    , try linkParser
    , paragraphParser
    ]

-- Convert MarkdownElement to HTML
elementToHTML :: MarkdownElement -> String
elementToHTML (Header level content) = "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"
elementToHTML (Paragraph content) = "<p>" ++ content ++ "</p>"
elementToHTML (UnorderedList items) = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") items ++ "</ul>"
elementToHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

-- Main function to parse Markdown and convert to HTML
markdownToHTML :: String -> Either ParseError String
markdownToHTML input = do
    elements <- parse markdownParser "" input
    return $ intercalate "\n" (map elementToHTML elements)

-- Function to read from a Markdown file and write HTML to an output file
processMarkdownFile :: FilePath -> FilePath -> IO ()
processMarkdownFile inputFile outputFile = do
    markdownText <- readFile inputFile
    case markdownToHTML markdownText of
        Left err -> putStrLn $ "Error parsing Markdown: " ++ show err
        Right html -> do
            writeFile outputFile html
            putStrLn $ "HTML written to: " ++ outputFile

-- Sample usage
main :: IO ()
main = do
    putStrLn "Enter the input Markdown file path:"
    inputFile <- getLine
    putStrLn "Enter the output HTML file path:"
    outputFile <- getLine
    processMarkdownFile inputFile outputFile