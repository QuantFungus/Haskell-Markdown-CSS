{-# LANGUAGE OverloadedStrings #-}

module Test where

import Test.HUnit
import Project

-- Test cases

-- Test for a header element
testHeader :: Test
testHeader = TestCase $ do
    let input = "# Header 1\n"
    let expectedOutput = "<h1>Header 1</h1>"
    case markdownToHTML input of
        Right result -> assertEqual "Header test failed" expectedOutput result
        Left err -> assertFailure $ "Parsing error: " ++ show err

-- Test for a paragraph element
testParagraph :: Test
testParagraph = TestCase $ do
    let input = "This is a paragraph.\n"
    let expectedOutput = "<p>This is a paragraph.</p>"
    case markdownToHTML input of
        Right result -> assertEqual "Paragraph test failed" expectedOutput result
        Left err -> assertFailure $ "Parsing error: " ++ show err

-- Test for an unordered list
testUnorderedList :: Test
testUnorderedList = TestCase $ do
    --NEED TO FILL

-- Test for a link
testLink :: Test
testLink = TestCase $ do
    --NEED TO FILL

-- Group all tests
tests :: Test
tests = TestList [ TestLabel "Header Test" testHeader
                 , TestLabel "Paragraph Test" testParagraph
                 , TestLabel "Unordered List Test" testUnorderedList
                 , TestLabel "Link Test" testLink
                 , TestLabel "Combined Elements Test" testCombinedElements
                 ]

-- Run tests with main
main :: IO Counts
main = runTestTT tests