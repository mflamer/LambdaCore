
module LamCor.Parser
(parse
,clex
,expr
,split
)where

import LamCor.Language

import Data.Char
import Data.Word

type Token = String

clex :: String -> [Token]
clex (c:cs)    
   | isSpace c = clex cs
   | isNumber c = num_token : clex rest_dig      
   | isAlpha c = var_tok : clex rest_alp 
      where 
         var_tok = c : takeWhile isIdChar cs
         rest_alp = dropWhile isIdChar cs         
         num_token = c : takeWhile isNumber cs
         rest_dig = dropWhile isNumber cs      
clex (c:d:cs)  
   | elem cd twoCharOps = cd : clex cs
      where cd = c : [d]              
clex (c:cs) = [c] : clex cs  
clex [] = []  

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isNumber c || (c == '_')        

twoCharOps :: [Token]
twoCharOps = ["==", "˜=", ">=", "<=", "->"]




keywords = ["def", "data", "let", "letrec", "case", "if", "lam", "Pack"]


chain :: ([Expr],[Token]) -> ([Expr],[Token])
chain ([e],[]) = ([e],[])
chain (es,[]) = (appl es,[])
chain (es,ts) = chain (es++es',ts') where
   (es',ts') = expr ts 


expr :: [Token] -> ([Expr],[Token])
expr [] = ([],[])
expr t@("(":ts) = (el, r) 
   where
   (l,r) = split ([],t) 0  
   ll = expr l
   (el,_) = chain ll
   -- rr = expr r
   -- (er,_) = chain rr
expr (")":ts)      = expr ts     
expr ("def":s:ts)  = ([DEF s e],ts') where
   (e:_,ts') = expr ts   
expr ("lam":s:ts)  = ([LAM s e],ts') where
   (e:_,ts') = expr ts
expr ("let":s:ts)  = ([LET s e e'],ts'') where
   (e:_,ts')   = expr ts
   (e':es',ts'') = expr ts'
expr ("letrec":s:ts)  = ([LETREC s e e'],ts'') where
   (e:_,ts')   = expr ts
   (e':es',ts'') = expr ts'
expr ("if":ts)     = ([IF eb et ef],ts''') where
   (eb:_,ts')   = expr ts
   (et:_,ts'')  = expr ts'
   (ef:_,ts''') = expr ts''
expr ("+":ts)      = let 
   (e:_, ts'@(t:tt)) = expr ts
   (e':_,ts'')       = expr ts' 
      in case t of ")" -> ([LAM "y" (PRIM_2 Add e (VAR "y"))],ts'')
                   x   -> ([PRIM_2 Add e e'],ts'')
expr ("-":ts)      = let 
   (e:_, ts'@(t:tt)) = expr ts
   (e':_,ts'')       = expr ts' 
      in case t of ")" -> ([LAM "y" (PRIM_2 Sub e (VAR "y"))],ts'')
                   x   -> ([PRIM_2 Sub e e'],ts'')                         
expr (t@(c:cs):ts) 
   | isNumber c = ([CONST (read t :: Word32)],ts)
   | otherwise  = ([VAR t],ts)   


split :: ([Token],[Token]) -> Int -> ([Token],[Token])
split (l,[]) d       = (reverse l,[])
split (l,"(":ts) d   
   | d == 0    = split (l, ts) (d+1)
   | otherwise = split ("(":l, ts) (d+1)
split (l,")":ts) d
   | d == 1    = (reverse l, ts)
   | otherwise = split (")":l, ts) (d-1) 
split (l,t:ts) d = split (t:l,ts) d
    


appl :: [Expr] -> [Expr]
appl []  = []
appl [e] = [e]
appl a@(e:es) = [APP a]     



-- syntaxTop :: ([Token],[Expr]) -> ([Token],[Expr])   




parse :: String -> [Expr]
parse s = es where (es, ts) = expr (clex s)
   