import Text.ParserCombinators.Parsec

-- Implemented following: https://book.realworldhaskell.org/read/using-parsec.html

-- a CSV file contains 0 or more lines
csvFile :: GenParser Char st [[String]]
csvFile =
    do 
        result <- many line
        eof
        return result

-- A line contains 1 or more cells
line :: GenParser Char st [String]
line = 
    do 
        result <- cells
        eol
        return result

-- build up a list of cells
cells :: GenParser Char st [String]
cells = 
    do
        first <- cellContent
        next <- remainingCells
        return (first : next)

-- either the cell ends with a comma, indicating more cells
-- or it doesn't indicating the end of a line
remainingCells :: GenParser Char st [String]
remainingCells = 
    (char ',' >> cells)     -- if comma, more cells
    <|> (return [])         -- else no more cells, empty list

-- each cell has some content, cannot be , or newline
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf ",\n")

-- newline character is \n
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

-- >>> parseCSV "c1,c2,c3\nr1c1,r1c2,r1c3\nr2c1,r2c2,r2c3\n"
-- Right [["c1","c2","c3"],["r1c1","r1c2","r1c3"],["r2c1","r2c2","r2c3"]]
