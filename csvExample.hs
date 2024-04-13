import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile =
    do result <- many line
        eof
        return result

line :: GenParser Char st [String]
line = undefined

