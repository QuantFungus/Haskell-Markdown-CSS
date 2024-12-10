{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Proj (markdownToHTML)  -- Import the function you want to test from Proj.hs
import Data.List (isInfixOf)

-- Test for a header element
testHeader :: Test
testHeader = TestCase $ do
    let input = "# Header 1\n"
    let expectedOutput = "<h1>Header 1</h1>"
    case markdownToHTML input of
        Right result -> assertEqual "Header test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for a paragraph element
testParagraph :: Test
testParagraph = TestCase $ do
    let input = "This is a paragraph.\n"
    let expectedOutput = "<p>This is a paragraph.</p>"
    case markdownToHTML input of
        Right result -> assertEqual "Paragraph test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for an unordered list
testUnorderedList :: Test
testUnorderedList = TestCase $ do
    let input = "- Item 1\n- Item 2\n- Item 3\n"
    let expectedOutput = "<ul><li>Item 1</li><li>Item 2</li><li>Item 3</li></ul>"
    case markdownToHTML input of
        Right result -> assertEqual "Unordered list test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for a link
testLink :: Test
testLink = TestCase $ do
    let input = "[Google](https://google.com)\n"
    let expectedOutput = "<p><a href=\"https://google.com\">Google</a></p>"
    case markdownToHTML input of
        Right result -> assertEqual "Link test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for combined elements
testCombinedElements :: Test
testCombinedElements = TestCase $ do
    let input = "# Header 1\n\nThis is a paragraph.\n\n- Item 1\n- Item 2\n\n[Google](https://google.com)\n"
    let expectedOutput = concat
            [ "<h1>Header 1</h1>\n"
            , "<p>This is a paragraph.</p>\n"
            , "<ul><li>Item 1</li><li>Item 2</li></ul>\n"
            , "<p><a href=\"https://google.com\">Google</a></p>"
            ]
    case markdownToHTML input of
        Right result -> assertEqual "Combined elements test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for invalid Markdown
testInvalidMarkdown :: Test
testInvalidMarkdown = TestCase $ do
    let input = "# Header 1\n\nThis is a paragraph with an unclosed [link(https://google.com)\n"
    case markdownToHTML input of
        Right _ -> assertFailure "Expected parsing to fail for invalid Markdown"
        Left err -> assertBool "Invalid Markdown should produce an error" ("Invalid Markdown" `isInfixOf` err)

tests :: Test
tests = TestList [ TestLabel "Header Test" testHeader
                 , TestLabel "Paragraph Test" testParagraph
                 , TestLabel "Unordered List Test" testUnorderedList
                 , TestLabel "Link Test" testLink
                 , TestLabel "Combined Elements Test" testCombinedElements
                 , TestLabel "Invalid Markdown Test" testInvalidMarkdown
                 ]

main :: IO Counts
main = runTestTT tests