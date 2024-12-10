{-# LANGUAGE OverloadedStrings #-}

module Proj where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate)
import System.IO

-- Data type to represent different Markdown elements
data MarkdownElement = Header Int String
                     | Paragraph String
                     | UnorderedList [MarkdownElement]
                     deriving (Show)

-- Parse a header (e.g., # Header 1)
headerParser :: Parser MarkdownElement
headerParser = do
    hashes <- many1 (char '#')
    space
    content <- manyTill anyChar (try newline <|> eofHandler)
    return $ Header (length hashes) content

-- Parse a paragraph with inline links
paragraphParser :: Parser MarkdownElement
paragraphParser = do
    content <- many1 (try linkInlineParser <|> plainTextParser)
    _ <- optional newline
    return $ Paragraph (concat content)

-- Parse an unordered list, including nested lists
unorderedListParser :: Parser MarkdownElement
unorderedListParser = do
    items <- many1 listItemParser
    return $ UnorderedList items

-- Parse a single list item and detect nested lists
listItemParser :: Parser MarkdownElement
listItemParser = do
    _ <- char '-'
    space
    content <- many1 (try linkInlineParser <|> plainTextParser)
    nested <- option [] nestedListParser
    let currentItem = Paragraph (concat content)
    if null nested
        then return currentItem
        else return $ UnorderedList (currentItem : nested)

-- Parse nested unordered lists (indentation is used to detect nesting)
nestedListParser :: Parser [MarkdownElement]
nestedListParser = do
    _ <- newline
    many1 $ try $ do
        spaces <- many1 (char ' ')
        if length spaces >= 4 -- Ensure proper indentation for nesting
            then listItemParser
            else fail "Not a nested item"

-- Parse inline links inside text
linkInlineParser :: Parser String
linkInlineParser = do
    _ <- char '['
    text <- manyTill anyChar (char ']')
    _ <- char '('
    url <- manyTill anyChar (char ')')
    return $ "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

-- Parse plain text
plainTextParser :: Parser String
plainTextParser = many1 (noneOf "[]\n") -- Ensure non-empty text

-- Handle end-of-input explicitly by returning a dummy Char
eofHandler :: Parser Char
eofHandler = lookAhead eof >> return '\n'

-- Skip blank lines
skipBlankLines :: Parser ()
skipBlankLines = skipMany (try (newline >> notFollowedBy (noneOf "\n")))

-- Combine all parsers into a single parser
markdownParser :: Parser [MarkdownElement]
markdownParser = many1 $ do
    skipBlankLines
    choice
        [ try headerParser
        , try unorderedListParser
        , paragraphParser
        ]

-- Convert MarkdownElement to HTML
elementToHTML :: MarkdownElement -> String
elementToHTML (Header level content) = "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"
elementToHTML (Paragraph content) = "<p>" ++ content ++ "</p>"
elementToHTML (UnorderedList items) = "<ul>" ++ concatMap (\item -> "<li>" ++ itemToHTML item ++ "</li>") items ++ "</ul>"

-- Convert list item to HTML (handling nested elements)
itemToHTML :: MarkdownElement -> String
itemToHTML (Paragraph content) = content
itemToHTML (UnorderedList items) = "<ul>" ++ concatMap (\item -> "<li>" ++ itemToHTML item ++ "</li>") items ++ "</ul>"
itemToHTML elem = elementToHTML elem

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