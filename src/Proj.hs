{-# LANGUAGE OverloadedStrings #-}

module Proj where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (intercalate)


-- Data type to represent different Markdown elements
data MarkdownElement = Header Int String
                     | Paragraph String
                     | UnorderedList [String]
                     deriving (Show)

-- Parse a header (e.g., # Header 1)
headerParser :: Parser MarkdownElement
headerParser = do
    hashes <- many1 (char '#')
    _ <- many1 space
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

-- Handle end-of-input explicitly by returning a dummy Char
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
    many $ do
        skipBlankLines
        choice
            [ try headerParser
            , try unorderedListParser
            , paragraphParser
            ] <* skipBlankLines

-- Convert MarkdownElement to HTML with proper formatting
elementToHTML :: MarkdownElement -> String
elementToHTML (Header level content) = "<h" ++ show level ++ ">" ++ content ++ "</h" ++ show level ++ ">"
elementToHTML (Paragraph content) = "<p>" ++ content ++ "</p>"
elementToHTML (UnorderedList items) =
    "<ul>" ++ intercalate "" (map (\item -> "<li>" ++ item ++ "</li>") items) ++ "</ul>"

markdownToHTML :: String -> Either ParseError String
markdownToHTML input = do
    elements <- parse (markdownParser <* eof) "" input
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