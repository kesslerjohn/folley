import Text.ParserCombinators.Parsec

-- Implemented following: https://book.realworldhaskell.org/read/using-parsec.html

csvFile = endBy line eol 
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input 