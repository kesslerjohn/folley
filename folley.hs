import Text.ParserCombinators.Parsec

{-- example of input format will follow --
c Comment 1
c Comment 2
c etc. 
p cnf 4 3 //format {# variables} {# clauses}
1 3 -4 0
4 0 2
3

represents (x1 | x3 | -x4) & (x4) & (x2 | x3)

-- example of output format --
c Comment 1
c Comment 2
s SATISFIABLE // 
v 1 2 3 4 // or {v -1 2 3 4}, {v -1 -2 3 4}, etc.
--}

{--
should take a string and give me a list of lists of literals
where each literal must be a string to account for negation

I'll assume no newlines in the input first for simplicity
except the file delimiter
--}

-- I want cnfSAT to return a list of clauses
-- actually isomorphic to csv
fCNF :: GenParser Char st [[String]]
fCNF = endBy line eol

line = sepBy clause (char '0')

clause = many (noneOf "0\n")

eol = char '\n'

parseCNF :: String -> Either ParseError [[String]]
parseCNF input = parse fCNF "(unknown)" input 

unSpace :: Either ParseError [Char]
unSpace = undefined

fHead :: Either ParseError [[String]] -> [String]
fHead (Left err) = ["(unknown)"]
fHead (Right l) = case l of
    [] -> ["Empty"]
    a:as -> head l

-- I have:
-- >>> fHead $ parseCNF "1 2 -3 0 1 4 0 -4\n"
-- ["1 2 -3 "," 1 4 "," -4"]
-- which is [String]
-- I want [["1", "2", "-3"], ["1", "4"], ["-4"]]
-- which is [[String]]

vars :: String -> [String]
vars [' '] = []
vars (' ' : as) = vars as
vars ('-' : bs : ' ' : []) = ["-" ++ [bs]]
vars (a : ' ' : as) = [[a]] ++ vars as
vars _ = []

-- >>> vars " q 4 5 -3 "
-- ["q","4","5","-3"]

--splitVars :: [String] -> [[String]]


-- reWrap: turns the output list of string into a list of lists
reWrap :: [String] -> [[String]]
reWrap [] = []
reWrap (a:as) = [[a]] ++ reWrap as

-- >>> "three" ++ " tired"
-- "three tired"
