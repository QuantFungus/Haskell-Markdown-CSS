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