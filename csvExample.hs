import Text.ParserCombinators.Parsec

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


