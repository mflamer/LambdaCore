
module LamCor.Language
(Expr(..)
,POp(..) 
,compile
,compileT
,genOPs
,exprToDB
,tailCallOpt
,packOpt
)
  where

import Data.Word
import Data.List
import Data.Bits
import qualified Data.Map as M

type Symb = String

data Expr = Const Int 
          | Def Symb Expr                      
          | App [Expr]
          | Lam Symb Expr
          | Let Symb Expr Expr
          | LetRec Symb Expr Expr
          | If Expr Expr Expr          
          | Var Symb
          | Get Symb
          | Set Symb Int
          ------------------------------
          | Prim_1 POp Expr
          | Prim_2 POp Expr Expr 
          -- | Inst Int        
          deriving (Eq, Show)


data DBExpr = DB_CONST Int                             
            | DB_APP Bool [DBExpr]
            | DB_LAM DBExpr
            | DB_LET DBExpr DBExpr
            | DB_LETREC DBExpr DBExpr
            | DB_IF DBExpr DBExpr DBExpr
            | DB_VAR Symb
            | DB_GET Symb
            | DB_SET Symb Int
            | DB_CALL Int
            | DB_JUMP Int
            | DB_I Int
            | DB_PRIM_1 POp DBExpr
            | DB_PRIM_2 POp DBExpr DBExpr
            | DB_INST String        
            deriving (Eq, Show)

data SF a = FF | SS a
    deriving (Eq, Show)


exprToDB :: Expr -> DBExpr
exprToDB expr = db_translate [] expr where
       db_translate _ (Const n) = DB_CONST n       
       db_translate env (Lam v e) = DB_LAM e' where
           e' = db_translate (v:env) e
       db_translate env (Let v e0 e1) = DB_LET e0' e1' where
           e0' = db_translate (v:env) e0
           e1' = db_translate (v:env) e1
       db_translate env (LetRec v e0 e1) = DB_LETREC e0' e1' where
           e0' = db_translate (v:env) e0     
           e1' = db_translate (v:env) e1   
       db_translate env (If b t e) = DB_IF b' t' e' where
           b' = db_translate env b
           t' = db_translate env t
           e' = db_translate env e     
       db_translate env (App ls) = DB_APP False ls' where
           ls' = map (db_translate env) ls    
       db_translate env (Prim_1 i e) = DB_PRIM_1 i e' where
           e' = db_translate env e
       db_translate env (Prim_2 i e0 e1) = DB_PRIM_2 i e0' e1' where
           e0' = db_translate env e0
           e1' = db_translate env e1         
       db_translate env (Var v)
        = case (find v env) of
            SS n -> DB_I n
            FF -> DB_INST v
        where 
            find v [] = FF
            find v (v':rest) | v == v' = SS 0
                             | otherwise = case find v rest of
                                            FF -> FF
                                            SS n -> SS (n+1)


tailCallOpt :: Bool -> DBExpr -> DBExpr
tailCallOpt _ (DB_LAM e)        = (DB_LAM (tailCallOpt False e))                           
tailCallOpt _ (DB_LET e0 e1)    = (DB_LET e0 (tailCallOpt False e1))
tailCallOpt _ (DB_LETREC e0 e1) = (DB_LETREC e0 (tailCallOpt False e1))
tailCallOpt _ (DB_IF b t e)     = (DB_IF b (tailCallOpt False t) (tailCallOpt False e))
tailCallOpt True (DB_APP _ es)  = (DB_APP False es)
tailCallOpt False (DB_APP _ es)  = (DB_APP True es)
tailCallOpt _ e                   = e


packOpt :: [Inst] -> [Inst]
-- PackOpt ((INST a):APPT:is) = (JUMP a):(packOpt is)
packOpt ((CLOS a):is) = (CLOS (packOpt a)):(packOpt is) 
packOpt ((IF a):is) = (IF (packOpt a)):(packOpt is)  
--packOpt ((LDI x):PUSH:is) = (LDIP x):(packOpt is) 
--packOpt ((ACC x):PUSH:is) = (ACCP x):(packOpt is) 
packOpt (RET:RET:is) = RET:(packOpt is) 
packOpt (i:is) = i:(packOpt is)
packOpt [] = []


-- ZAM --

data Inst = NOP
          | LDI Int          
          | CLOS [Inst]
          | IF [Inst]
          | RET
          | ACC Int
          | APPT
          | APP
          | PUSH
          | MARK
          | GRAB
          | LET
          | ELET
          | TEMP
          | UPDT
          | END
          | PRIM POp          
          | INST String      
          deriving (Eq, Show)

data POp = Add      
         | Sub      
         | Mul                
         | And     
         | Or 
         | Not
         | Xor          
         | Xnor
         | Ashr
         | Lshr
         | Lshl          
         | Eq
         | Ne 
         | Gt 
         | Lt 
         | Gte
         | Lte 
         | Abv
         | Bel
         deriving (Eq, Show)




compile :: DBExpr -> [Inst]
compile (DB_INST ins) = [INST ins] 
compile (DB_CONST k) = [LDI k]
compile (DB_I n) = [ACC n]
compile (DB_APP False es) = MARK:es' ++ [APP] 
                                where es' = intercalate [PUSH] $ map compile $ reverse es 
compile e@(DB_APP True es) = compileT e                                                                      
compile (DB_LAM dbl) = [CLOS $ compileT dbl ++ [RET]] 
compile (DB_LET v e) = (compile v) ++ LET:(compile e) ++ [ELET]  
compile (DB_LETREC v e) = TEMP:(compile v) ++ UPDT:(compile e) ++ [ELET] 
compile (DB_IF b t e) = (compile b) ++ [IF ((compile t) ++ [RET])]  ++ (compile e) ++ [RET] 
compile (DB_PRIM_1 i e) = (compile e) ++ [PRIM i]
compile (DB_PRIM_2 i e0 e1) = (compile e1) ++ PUSH:(compile e0) ++ [PRIM i]                                


compileT :: DBExpr -> [Inst]
compileT (DB_LAM dbl) = GRAB:(compileT dbl)
compileT (DB_LET v e) = (compile v) ++ LET:(compileT e)  
compileT (DB_LETREC v e) = TEMP:(compile v) ++ UPDT:(compileT e)  
compileT (DB_APP _ dbls) = dbls' ++ [APPT] 
                                where dbls' = intercalate [PUSH] $ map compile $ reverse dbls
compileT l = compile l



----------------------------------------------------------------------------------------------
-- CodeGen ---------------------------------------------------------------------------------

genOPs :: [Inst] -> ([String], Int) -> ([String], Int)
genOPs (NOP:ins)         (ops,i)  = genOPs ins ("NOP":ops, i+1)
genOPs ((ACC 0):ins)    (ops,i)  = genOPs ins ("ACC0":ops, i+1)
genOPs ((ACC db):ins)   (ops,i)  = genOPs ins (("ACC "++(show db)):ops, i+1) 
genOPs (APPT:ins)        (ops,i)  = genOPs ins ("APPT":ops, i+1)
genOPs (APP:ins)         (ops,i)  = genOPs ins ("APP":ops, i+1)    
genOPs (PUSH:ins)        (ops,i)  = genOPs ins ("PUSH":ops, i+1)    
genOPs (MARK:ins)        (ops,i)  = genOPs ins ("MARK":ops, i+1)                               
genOPs (GRAB:ins)        (ops,i)  = genOPs ins ("GRAB":ops, i+1)
genOPs (RET:ins)         (ops,i)  = genOPs ins ("RET":ops, i+1)    
genOPs (LET:ins)         (ops,i)  = genOPs ins ("LET":ops, i+1)
genOPs (ELET:ins)        (ops,i)  = genOPs ins ("ELET":ops, i+1)
genOPs (TEMP:ins)        (ops,i)  = genOPs ins ("TEMP":ops, i+1)
genOPs (UPDT:ins)        (ops,i)  = genOPs ins ("UPDT":ops, i+1)
genOPs ((LDI x):ins)     (ops,i)  
  | x .&. 0xFFFF0000 == 0         = genOPs ins ((("LDIS "++(show x)):ops), i+1)
  | otherwise                     = genOPs ins ((("LDIL "++(show x)):ops), i+1) 
genOPs ((PRIM Add):ins)  (ops,i)  = genOPs ins ("ADD":ops, i+1) 
genOPs ((PRIM Sub):ins)  (ops,i)  = genOPs ins ("SUB":ops, i+1)
genOPs ((PRIM Mul):ins)  (ops,i)  = genOPs ins ("MUL":ops, i+1)
genOPs ((PRIM And):ins)  (ops,i)  = genOPs ins ("AND":ops, i+1)
genOPs ((PRIM Or):ins)   (ops,i)  = genOPs ins ("OR":ops, i+1)
genOPs ((PRIM Not):ins)  (ops,i)  = genOPs ins ("NOT":ops, i+1)
genOPs ((PRIM Xor):ins)  (ops,i)  = genOPs ins ("XOR":ops, i+1)
genOPs ((PRIM Xnor):ins) (ops,i)  = genOPs ins ("XNOR":ops, i+1)
genOPs ((PRIM Ashr):ins) (ops,i)  = genOPs ins ("ASHR":ops, i+1)
genOPs ((PRIM Lshr):ins) (ops,i)  = genOPs ins ("LSHR":ops, i+1)
genOPs ((PRIM Lshl):ins) (ops,i)  = genOPs ins ("ASHL":ops, i+1)
genOPs ((PRIM Eq):ins)   (ops,i)  = genOPs ins ("EQ":ops, i+1)
genOPs ((PRIM Ne):ins)   (ops,i)  = genOPs ins ("NE":ops, i+1)
genOPs ((PRIM Gt):ins)   (ops,i)  = genOPs ins ("GT":ops, i+1)
genOPs ((PRIM Lt):ins)   (ops,i)  = genOPs ins ("LT":ops, i+1)
genOPs ((PRIM Gte):ins)  (ops,i)  = genOPs ins ("GTE":ops, i+1)
genOPs ((PRIM Lte):ins)  (ops,i)  = genOPs ins ("LTE":ops, i+1)
genOPs ((PRIM Abv):ins)  (ops,i)  = genOPs ins ("ABV":ops, i+1)
genOPs ((PRIM Bel):ins)  (ops,i)  = genOPs ins ("BEL":ops, i+1)

genOPs ((INST xs):ins)    (ops,i) = genOPs ins (("CLOS "++xs):ops, i+1)

genOPs ((CLOS c):[])      (ops,i) = (ops'++(op:ops), i') 
  where (ops', i')  = genOPs c (ops, i+1)       
        op = "CLOS "++label
        label = "c"++(show i)      

genOPs ((CLOS c):ins)     (ops,i) = (ops''++(op:ops), i'') 
  where (ops', i') = genOPs ins ([], i+1)
        (ops'', i'') = genOPs c (label:ops', i')        
        op = "CLOS "++label
        label = "c"++(show i)   

genOPs ((IF c):ins)     (ops,i) = (ops''++(op:ops), i'') 
  where (ops', i')   = genOPs ins ([], i+1)
        (ops'', i'') = genOPs c ((label++":"):ops', i') 
        label = "if"++(show i)       
        op = "IF "++label       
genOPs []                (ops@("RET":_),i) = (ops, i) -- no need for End after RETC 
genOPs []                (ops@("APPT":_),i) = (ops, i) -- no need for End after AppT
genOPs []                (ops,i) = ("END":ops, i+1) 


