{-# LANGUAGE OverloadedStrings #-}

module Proj where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate, isInfixOf)

-- Data type to represent different Markdown elements
data MarkdownElement = Header Int String
                     | Paragraph String
                     | UnorderedList [String]
                     deriving (Show)

-- Parse a header (e.g., # Header 1)
headerParser :: Parser MarkdownElement
headerParser = do
    hashes <- many1 (char '#')
    _ <- space
    content <- manyTill anyChar (try newline <|> eofHandler)
    return $ Header (length hashes) content

-- Parse a paragraph
paragraphParser :: Parser MarkdownElement
paragraphParser = do
    content <- many1 (try linkInlineParser <|> plainTextParser)
    _ <- try newline <|> eofHandler
    return $ Paragraph (concat content)

-- Parse an unordered list
unorderedListParser :: Parser MarkdownElement
unorderedListParser = do
    items <- many1 listItemParser
    return $ UnorderedList (map trim items)

-- Parse a single list item
listItemParser :: Parser String
listItemParser = do
    _ <- char '-'
    _ <- space
    manyTill anyChar (try newline <|> eofHandler)

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

-- Handle end-of-input explicitly for Char
eofHandler :: Parser Char
eofHandler = lookAhead eof >> return '\n'

-- Utility function to trim whitespace from beginning and end
trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Skip blank lines
skipBlankLines :: Parser ()
skipBlankLines = skipMany (try (newline >> notFollowedBy (oneOf "\n")))

-- Combine all parsers into a single parser
markdownParser :: Parser [MarkdownElement]
markdownParser = do
    skipBlankLines
    elements <- many $ do
        element <- choice
            [ try headerParser
            , try unorderedListParser
            , paragraphParser
            ]
        skipBlankLines
        return element
    eof -- Ensure that the entire input is parsed
    return elements

-- Validate Markdown
validateMarkdown :: String -> Either String [MarkdownElement]
validateMarkdown input =
    case parse markdownParser "" input of
        Left err -> Left $ "Invalid Markdown: " ++ show err
        Right elements -> Right elements

-- Suggest fixes for invalid Markdown
suggestFix :: String -> String
suggestFix errorMsg
  | "Unclosed" `isInfixOf` errorMsg = "Ensure all elements are properly closed (e.g., **bold**, [links])."
  | "Header missing space" `isInfixOf` errorMsg = "Add a space after the `#` symbol in headers."
  | "Invalid list item" `isInfixOf` errorMsg = "Start list items with `-` or `*`."
  | otherwise = "Check your Markdown syntax for errors."

-- Convert MarkdownElement to HTML with proper formatting
elementToHTML :: MarkdownElement -> String
elementToHTML (Header level content) = "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"
elementToHTML (Paragraph content) = "<p>" ++ content ++ "</p>"
elementToHTML (UnorderedList items) =
    "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") items ++ "</ul>"

-- Main function to parse Markdown and convert to HTML
markdownToHTML :: String -> Either String String
markdownToHTML input = do
    elements <- validateMarkdown input
    return $ intercalate "\n" (map elementToHTML elements)

-- Function to read from a Markdown file and write HTML to an output file
processMarkdownFile :: FilePath -> FilePath -> IO ()
processMarkdownFile inputFile outputFile = do
    markdownText <- readFile inputFile
    case markdownToHTML markdownText of
        Left err -> do
            putStrLn $ "Error detected: " ++ err
            putStrLn $ "Suggestion: " ++ suggestFix err
        Right html -> do
            writeFile outputFile html
            putStrLn $ "HTML written to: " ++ outputFile


-- Only in use if ran with GHCI, cabal uses app/Main.hs instead
main :: IO ()
main = do
    putStrLn "Enter the input Markdown file path:"
    inputFile <- getLine
    putStrLn "Enter the output HTML file path:"
    outputFile <- getLine
    processMarkdownFile inputFile outputFile