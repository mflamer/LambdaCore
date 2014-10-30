module Parser
(parse
,clex
)where


import Data.Char


type Token = String

clex :: String -> [Token]
clex (c:cs) 
   | isSpace c = clex cs
   | isDigit c = num_token : clex rest_dig      
   | isAlpha c = var_tok : clex rest_alp 
      where 
         var_tok = c : takeWhile isIdChar cs
         rest_alp = dropWhile isIdChar cs         
         num_token = c : takeWhile isDigit cs
         rest_dig = dropWhile isDigit cs 
           
clex (c:d:cs)  
   | elem cd twoCharOps = cd : clex cs
      where cd = c : [d]              
clex (c:cs) = [c] : clex cs  
clex [] = []  

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')        

twoCharOps :: [Token]
twoCharOps = ["==", "˜=", ">=", "<=", "->"]




keywords = ["where", "data", "let", "letrec", "case", "in", "of", "Pack"]