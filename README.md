# Haskell-Markdown-HTML

## How to run the converter

 1. Use `cd` to navigate to the project root
 2. Run `cabal run Haskell-Markdown-HTML`
 3. When prompted, type the name of an input Markdown file
 4. When prompted, type the name of an output HTML file
 5. The program will output a converted HTML file (If the Markdown filename was invalid then no output file will be made)

Cabal looks for `app/Main.hs`, which then looks for` src/Proj.hs`'s processMarkdownFile method. 

## How to run the tests

 1. Use `cd` to navigate to the project root
 2. Run `cabal run Haskell-Markdown-HTML-test`
 3. All the tests will be ran, and the results will be outputted to the terminal

Cabal looks for `test/Main.hs`, which then looks for` src/Proj.hs`'s markdownToHTML method. 

## Main Components

- **src/Proj.hs** - Contains the core logic for parsing Markdown and converting it to HTML:
  - `MarkdownElement` data type for representing different Markdown elements.
  - Parsers for headers, paragraphs, lists, and links.
  - `markdownToHTML` function to convert Markdown to HTML.
  - `processMarkdownFile` function to handle file I/O operations.

- **app/Main.hs** - The main application entry point:
  - Uses `processMarkdownFile` from `Proj.hs` to read Markdown input, convert it, and write HTML output.

- **test/Main.hs** - Contains the test suite:
  - Uses HUnit for testing the `markdownToHTML` function from `Proj.hs`.

## Recommended Order of Review

1. **src/Proj.hs** - Understand the core functionality including parsing and conversion logic.
2. **app/Main.hs** - See how the conversion process is initiated.
3. **test/Main.hs** - Review the test cases to understand expected behaviors.

## Additional Libraries

This project uses:

- **parsec** - For parsing Markdown syntax.
- **HUnit** - For unit testing.

Make sure these libraries are installed or included in your cabal file.
