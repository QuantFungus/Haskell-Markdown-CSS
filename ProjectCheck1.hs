{-# LANGUAGE OverloadedStrings #-}

module MarkdownToHTML where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.List (intercalate)
import Data.Maybe (catMaybes)

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
-- unorderedListParser = do
    --NEED FUNCTIONALITY HERE
    -- return $ UnorderedList tems

-- Parse a link (e.g., [Link](https://example.com))
linkParser :: Parser MarkdownElement
-- linkParser = do
    --NEED FUNCTIONALITY HERE
    -- return $ Link text url

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

-- Sample function to run conversion
main :: IO ()
main = do
    let markdownText = "# Header 1\nThis is a paragraph.\n- Item 1\n- Item 2\n[Link](https://example.com)"
    case markdownToHTML markdownText of
        Left err -> print err
        Right html -> putStrLn html