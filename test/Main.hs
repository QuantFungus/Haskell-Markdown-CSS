{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Proj (markdownToHTML)  -- Import the function you want to test from Proj.hs

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
    let input = "[OpenAI](https://openai.com)\n"
    let expectedOutput = "<a href=\"https://openai.com\">OpenAI</a>"
    case markdownToHTML input of
        Right result -> assertEqual "Link test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

-- Test for combined elements
testCombinedElements :: Test
testCombinedElements = TestCase $ do
    let input = "# Header 1\n\nThis is a paragraph.\n\n- Item 1\n- Item 2\n\n[OpenAI](https://openai.com)\n"
    let expectedOutput = unlines
            [ "<h1>Header 1</h1>"
            , "<p>This is a paragraph.</p>"
            , "<ul><li>Item 1</li><li>Item 2</li></ul>"
            , "<a href=\"https://openai.com\">OpenAI</a>"
            ]
    case markdownToHTML input of
        Right result -> assertEqual "Combined elements test failed" expectedOutput result
        Left err     -> assertFailure $ "Parsing error: " ++ show err

tests :: Test
tests = TestList [ TestLabel "Header Test" testHeader
                 , TestLabel "Paragraph Test" testParagraph
                 , TestLabel "Unordered List Test" testUnorderedList
                 , TestLabel "Link Test" testLink
                 , TestLabel "Combined Elements Test" testCombinedElements
                 ]

main :: IO Counts
main = runTestTT tests