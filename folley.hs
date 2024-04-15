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
--}