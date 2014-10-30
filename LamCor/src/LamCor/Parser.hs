
module LamCor.Parser
(parse
,clex
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




syntax :: ([Token],[Expr]) -> ([Token],[Expr])
syntax (("(":ts), es) = syntax(ts, es) 
syntax ((")":ts), es) = syntax(ts, es) --where 
   --(ts', es') = syntax (ts, [])  
syntax (("def":s:ts), es) = syntax (ts', (DEF s e'):es) where
   (ts', e':es') = syntax (ts, [])
syntax (("lam":s:ts), es) = syntax (ts', (LAM s e'):es) where
   (ts', e':es') = syntax (ts, [])
syntax (("let":s:ts), es) = syntax (ts'', (LET s e' e''):es) where
   (ts', e':es') = syntax (ts, [])
   (ts'', e'':es'') = syntax (ts', []) 
syntax (("if":ts), es) = syntax (ts3, (IF eb et ef):es) where
   (ts1, eb:es1) = syntax (ts, [])
   (ts2, et:es2) = syntax (ts1, [])
   (ts3, ef:es3) = syntax (ts2, [])    
syntax (("+":ts), es) = let 
   (ts'@(t:tt), e':es') = syntax (ts, [])
   (ts'', e'':es'') = syntax (ts', [])  
      in case t of ")" -> syntax (ts', (LAM "y" (PRIM_2 Add e' (VAR "y")):es))
                   x -> syntax (ts'', (PRIM_2 Add e' e''):es)
syntax (("-":ts), es) = let 
   (ts'@(t:tt), e':es') = syntax (ts, [])
   (ts'', e'':es'') = syntax (ts', [])  
      in case t of ")" -> syntax (ts', (LAM "y" (PRIM_2 Sub e' (VAR "y")):es))
                   x -> syntax (ts'', (PRIM_2 Sub e' e''):es)                           
syntax (t@(c:cs):ts, es) 
   | isNumber c = (ts, (CONST (read t :: Word32)):es)
   | otherwise  = (ts, (VAR t):es)
syntax ([], es) = ([], es)    


parse :: String -> [Expr]
parse s = es where 
   (ts, es) = syntax ((clex s), [])
