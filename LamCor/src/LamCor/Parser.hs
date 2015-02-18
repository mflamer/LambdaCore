
module LamCor.Parser
(parse
,clex
,expr
,split
,compileSrc
,compileTop
,findMain
)where

import LamCor.Language

import Data.Char
import Data.Int
import Data.List
import qualified Data.Map as M

type Token = String

clex :: String -> [Token]
clex (c:d:cs)  
   | elem cd twoCharOps = cd : clex cs
   | cd == "--" = clex (restComment cs) 
      where cd = c : [d] 
clex (c:cs)    
   | isSpace c  = clex cs   
   | isNumber c || c == '-' = num_token : clex rest_dig      
   | isAlpha c  = var_tok : clex rest_alp 
   | isParen c  = [c] : clex cs
      where 
         var_tok = c : takeWhile isIdChar cs
         rest_alp = dropWhile isIdChar cs         
         num_token = c : takeWhile isNumber cs
         rest_dig = dropWhile isNumber cs 
         isParen c = (c == '(') || (c == ')')                 
clex (c:cs) = [c] : clex cs  
clex [] = []  

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isNumber c || (c == '_')        

twoCharOps :: [Token]
twoCharOps = ["==","!=",">=","<=","->","~^","/>",">>","<<"]

restComment :: String -> String 
restComment ('\n':ss) = ss
restComment (s:ss)    = restComment ss
restComment s         = s



-- keywords = ["def", "data", "let", "letrec",binaryOpsif", "lam", "Pack"]

unaryOps :: M.Map String POp
unaryOps = M.fromList
   [("~",Not)]

binaryOps :: M.Map String POp
binaryOps = M.fromList
   [("+",Add) 
   ,("-",Sub) 
   ,("*",Mul) 
   ,("&",And) 
   ,("|",Or)   
   ,("^",Xor) 
   ,("~^",Xnor)
   ,("/>",Ashr)
   ,(">>",Lshr)
   ,("<<",Lshl)
   ,("==",Eq)
   ,("!=",Ne) 
   ,(">",Gt) 
   ,("<",Lt) 
   ,(">=",Gte)
   ,("<=",Lte) 
   ,("abv",Abv)
   ,("bel",Bel)]


syntaxTop :: ([Expr],[Token]) -> ([Expr],[Token])   
syntaxTop (es,[]) = (es,[])
syntaxTop (es,ts) = syntaxTop (es ++ es', ts') where
   (es',ts') = expr ts


expr :: [Token] -> ([Expr],[Token])
expr [] = ([],[])
expr t@("(":ts) = (el, r) 
   where
   (l,r) = split ([],t) 0  
   ll = expr l
   (el,_) = chain ll
expr (")":ts)      = expr ts     
expr ("def":s:ts)  = ([Def s e],ts') where
   (e:_,ts') = expr ts   
expr ("lam":s:ts)  = ([Lam s e],ts') where
   (e:_,ts') = expr ts
expr ("let":s:ts)  = ([Let s e e'],ts'') where
   (e:_,ts')   = expr ts
   (e':es',ts'') = expr ts'
expr ("letrec":s:ts)  = ([LetRec s e e'],ts'') where
   (e:_,ts')   = expr ts
   (e':es',ts'') = expr ts'
expr ("if":ts)     = ([If eb et ef],ts''') where
   (eb:_,ts')   = expr ts
   (et:_,ts'')  = expr ts'
   (ef:_,ts''') = expr ts''                       
expr (t@(c:cs):ts) 
   | M.member t binaryOps = prim2 (t:ts)
   | M.member t unaryOps = prim1 (t:ts)
   | isNumber c || c == '-' = ([Const x],ts) 
   | otherwise  = ([Var t],ts)
   where x = (read t :: Int)

prim1 :: [Token] -> ([Expr],[Token])
prim1 (t:ts) = case ts of
   [] -> ([Lam "y" (Prim_1 op (Var "y"))],ts)
   x       -> ([Prim_1 op e1],ts')   
   where op          = unaryOps M.! t
         (e1:_,ts')  = expr ts         

prim2 :: [Token] -> ([Expr],[Token])
prim2 (t:ts) = case ts' of
   [] -> ([Lam "y" (Prim_2 op e1 (Var "y"))],ts')
   x       -> ([Prim_2 op e1 e2],ts'')   
   where op          = binaryOps M.! t
         (e1:_,ts')  = expr ts
         (e2:_,ts'') = expr ts'

split :: ([Token],[Token]) -> Int -> ([Token],[Token])
split (l,[]) d       = (reverse l,[])
split (l,"(":ts) d   
   | d == 0    = split (l, ts) (d+1)
   | otherwise = split ("(":l, ts) (d+1)
split (l,")":ts) d
   | d == 1    = (reverse l, ts)
   | otherwise = split (")":l, ts) (d-1) 
split (l,t:ts) d = split (t:l,ts) d

    
chain :: ([Expr],[Token]) -> ([Expr],[Token])
chain ([e],[]) = ([e],[])
chain (es,[]) = (appl es,[])
chain (es,ts) = chain (es++es',ts') where
   (es',ts') = expr ts


appl :: [Expr] -> [Expr]
appl []  = []
appl [e] = [e]
appl a@(e:es) = [App a]     


parse :: String -> [Expr]
parse s = es where (es, ts) = syntaxTop ([], clex s)  


findMain :: [Expr] -> [Expr]
findMain es = main++defs where 
   (main,defs) = takeDefs (es,[])

takeDefs :: ([Expr],[Expr]) -> ([Expr],[Expr])
takeDefs (e@(Def s _):es,es') = takeDefs (es,e:es')
takeDefs (es,es') = (es,es')  


compileTop :: ([String], Int) -> [Expr] -> ([String], Int)
compileTop (ins,pc) [] = (ins,pc)
compileTop (ins,pc) ((Def s e):es) = compileTop ((init ins')++(label:ins),pc') es where
   label = (s++":")
   (ins',pc')  = compileExpr e pc
compileTop (ins,pc) (e:es) = compileTop (ins'++ins,pc') es where
   (ins',pc')  = compileExpr e pc 
    

compileExpr :: Expr -> Int -> ([String], Int)
compileExpr e pc = genOPs (packOpt (compile $ tailCallOpt True $ exprToDB e)) ([],pc)


compileSrc :: String -> String
compileSrc src = concat (intersperse "\n" (reverse ins)) where
   (ins,pc) = compileTop ([],0) exprs
   exprs = findMain $ parse src