
module LamCor.Language
(Expr(..)
,POp(..) 
,SymbTable 
,compile
,compileT
,genOPs
,exprToDB
,tailCallOpt
)
  where

import Data.Word
import Data.List
import Data.Bits
import qualified Data.Map as M

type Symb = String
type SymbTable = M.Map Symb Word32

data Expr = CONST Word32 
          | DEF Symb Expr                      
          | APP [Expr]
          | LAM Symb Expr
          | LET Symb Expr Expr
          | LETREC Symb Expr Expr
          | IF Expr Expr Expr          
          | VAR Symb
          | GET Symb
          | SET Symb Word32
          ------------------------------
          | PRIM_1 POp Expr
          | PRIM_2 POp Expr Expr 
          | INST Word32        
          deriving (Eq, Show)


data DBExpr = DB_CONST Word32 
            -- | DB_DEF Symb DBExpr                              
            | DB_APP Bool [DBExpr]
            | DB_LAM DBExpr
            | DB_LET DBExpr DBExpr
            | DB_LETREC DBExpr DBExpr
            | DB_IF DBExpr DBExpr DBExpr
            | DB_VAR Symb
            | DB_GET Symb
            | DB_SET Symb Word32
            | DB_CALL Word32
            | DB_JUMP Word32
            | DB_I Word32
            | DB_PRIM_1 POp DBExpr
            | DB_PRIM_2 POp DBExpr DBExpr
            | DB_INST Word32          
            deriving (Eq, Show)

data SF a = FF | SS a
    deriving (Eq, Show)


exprToDB :: SymbTable -> Expr -> DBExpr
exprToDB symt l = db_translate [] l where
       -- db_translate env (DEF s e) = DB_DEF s e' where
       --     e' = db_translate env e 
       db_translate _ (CONST n) = DB_CONST n  
       db_translate _ (INST x) = DB_INST x      
       db_translate env (LAM v e) = DB_LAM e' where
           e' = db_translate (v:env) e
       db_translate env (LET v e0 e1) = DB_LET e0' e1' where
           e0' = db_translate (v:env) e0
           e1' = db_translate (v:env) e1
       db_translate env (LETREC v e0 e1) = DB_LETREC e0' e1' where
           e0' = db_translate (v:env) e0     
           e1' = db_translate (v:env) e1   
       db_translate env (IF b t e) = DB_IF b' t' e' where
           b' = db_translate env b
           t' = db_translate env t
           e' = db_translate env e     
       db_translate env (APP ls) = DB_APP False ls' where
           ls' = map (db_translate env) ls    
       db_translate env (PRIM_1 i e) = DB_PRIM_1 i e' where
           e' = db_translate env e
       db_translate env (PRIM_2 i e0 e1) = DB_PRIM_2 i e0' e1' where
           e0' = db_translate env e0
           e1' = db_translate env e1         
       db_translate env (VAR v)
        = case (find v env) of
            SS n -> DB_I n
            FF -> case (M.lookup v symt) of
                Nothing   -> DB_INST 0
                Just inst -> DB_INST inst
        where 
            find v [] = FF
            find v (v':rest) | v == v' = SS 0
                             | otherwise = case find v rest of
                                            FF -> FF
                                            SS n -> SS (n+1)


tailCallOpt ::  DBExpr -> DBExpr
tailCallOpt (DB_LAM e)        = (DB_LAM (tailCallOpt e))                           
tailCallOpt (DB_LET e0 e1)    = (DB_LET e0 (tailCallOpt e1))
tailCallOpt (DB_LETREC e0 e1) = (DB_LETREC e0 (tailCallOpt e1))
tailCallOpt (DB_IF b t e)     = (DB_IF b (tailCallOpt t) (tailCallOpt e))
tailCallOpt (DB_APP _ es)     = (DB_APP True es)
tailCallOpt e                 = e


-- ZAM --

data INST = Access Word32
          ------------
          | Appterm
          | Apply 
          | Push
          | Pushmark
          ------------ Application
          | Cur [INST]
          | Grab
          | RetC
          | Let
          | EndLet
          | Dummy
          | Update
          ------------ Absraction
          | If [INST]
          | Call Word32
          -- | Jump
          | Ret 
          -- | Get
          -- | Set
          | Prim POp
          | Const Word32
          | Inst Word32
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




compile :: DBExpr -> [INST]
compile (DB_INST i) = [Inst i] 
compile (DB_CONST k) = [Const k]
compile (DB_I n) = [Access n]
compile (DB_APP False es) = [Pushmark] ++ es' ++ [Apply] 
                                where es' = intercalate [Push] $ map compile $ reverse es 
compile e@(DB_APP True es) = compileT e                                                                      
compile (DB_LAM dbl) = [Cur $ compileT dbl ++ [RetC]] 
compile (DB_LET v e) = (compile v) ++ Let:(compile e) ++ [EndLet]  
compile (DB_LETREC v e) = Dummy:(compile v) ++ Update:(compile e) ++ [EndLet] 
compile (DB_IF b t e) = (compile b) ++ [If $ compile t] ++ (compile e) 
compile (DB_CALL a) = [Call a]
compile (DB_PRIM_1 i e) = (compile e) ++ [Prim i]
compile (DB_PRIM_2 i e0 e1) = (compile e1) ++ Push:(compile e0) ++ [Prim i]                                


compileT :: DBExpr -> [INST]
compileT (DB_LAM dbl) = Grab:(compileT dbl)
compileT (DB_LET v e) = (compile v) ++ Let:(compileT e)  
compileT (DB_LETREC v e) = Dummy:(compile v) ++ Update:(compileT e)  
compileT (DB_APP _ dbls) = dbls' ++ [Appterm] 
                                where dbls' = intercalate [Push] $ map compile $ reverse dbls
compileT l = compile l



----------------------------------------------------------------------------------------------
-- CodeGen ---------------------------------------------------------------------------------






genOPs :: [INST] -> ([Word32], Word32) -> ([Word32], Word32)
genOPs ((Access db):ins) (ops,i)  = genOPs ins ((0x48000000 .|. (db .&. 0x0000001F)):ops, i+1)
genOPs (Appterm:ins)     (ops,i)  = genOPs ins (0x404C02A0:ops, i+1)
genOPs (Apply:ins)       (ops,i)  = genOPs ins (0x405D02A0:ops, i+1)    
genOPs (Push:ins)        (ops,i)  = genOPs ins (0x44040000:ops, i+1)    
genOPs (Pushmark:ins)    (ops,i)  = genOPs ins (0x7C04F800:ops, i+1)                               
genOPs (Grab:ins)        (ops,i)  = genOPs ins (0x588E04C0:ops, i+1)
genOPs ((Call a):ins)    (ops,i)  = genOPs ins ((0x01A10000 .|. a):ops, i+1)  
genOPs (Ret:ins)         (ops,i)  = genOPs ins (0x01430000:ops, i+1)   
genOPs (RetC:ins)        (ops,i)  = genOPs ins (0x40CE06C0:ops, i+1) 
genOPs (Let:ins)         (ops,i)  = genOPs ins (0x40000120:ops, i+1)
genOPs (EndLet:ins)      (ops,i)  = genOPs ins (0x40000060:ops, i+1)
genOPs (Dummy:ins)       (ops,i)  = genOPs ins (0x7800F920:ops, i+1)
genOPs (Update:ins)      (ops,i)  = genOPs ins (0x40000100:ops, i+1)
genOPs ((Const x):ins)   (ops,i)  = genOPs ins ((0x80000000 .|. x):ops, i+1)  
genOPs ((Prim Add):ins)  (ops,i)  = genOPs ins (0x780C1100:ops, i+1) 
genOPs ((Prim Sub):ins)  (ops,i)  = genOPs ins (0x780C1900:ops, i+1)
genOPs ((Prim Mul):ins)  (ops,i)  = genOPs ins (0x780C2100:ops, i+1)
genOPs ((Prim And):ins)  (ops,i)  = genOPs ins (0x780C2900:ops, i+1)
genOPs ((Prim Or):ins)   (ops,i)  = genOPs ins (0x780C3100:ops, i+1)
genOPs ((Prim Not):ins)  (ops,i)  = genOPs ins (0x780C3900:ops, i+1)
genOPs ((Prim Xor):ins)  (ops,i)  = genOPs ins (0x780C4100:ops, i+1)
genOPs ((Prim Xnor):ins) (ops,i)  = genOPs ins (0x780C4900:ops, i+1)
genOPs ((Prim Ashr):ins) (ops,i)  = genOPs ins (0x780C5100:ops, i+1)
genOPs ((Prim Lshr):ins) (ops,i)  = genOPs ins (0x780C5900:ops, i+1)
genOPs ((Prim Lshl):ins) (ops,i)  = genOPs ins (0x780C6100:ops, i+1)
genOPs ((Prim Eq):ins)   (ops,i)  = genOPs ins (0x780C6900:ops, i+1)
genOPs ((Prim Ne):ins)   (ops,i)  = genOPs ins (0x780C7100:ops, i+1)
genOPs ((Prim Gt):ins)   (ops,i)  = genOPs ins (0x780C7900:ops, i+1)
genOPs ((Prim Lt):ins)   (ops,i)  = genOPs ins (0x780C8100:ops, i+1)
genOPs ((Prim Gte):ins)  (ops,i)  = genOPs ins (0x780C8900:ops, i+1)
genOPs ((Prim Lte):ins)  (ops,i)  = genOPs ins (0x780C9100:ops, i+1)
genOPs ((Prim Abv):ins)  (ops,i)  = genOPs ins (0x780C9900:ops, i+1)
genOPs ((Prim Bel):ins)  (ops,i)  = genOPs ins (0x780CA100:ops, i+1)

genOPs ((Inst x):ins)    (ops,i) = genOPs ins (x:ops, i+1) 
genOPs ((Cur c):[])      (ops,i) = (ops' ++ ((0x20000000 .|. i):ops), i') 
  where (ops', i') = genOPs c (ops, i) 

genOPs ((Cur c):ins)     (ops,i) = (ops'' ++ ((0x20000000 .|. i'):ops), i'') 
  where (ops', i') = genOPs ins ([], i+1)
        (ops'', i'') = genOPs c (ops', i') 

genOPs ((If c):ins)     (ops,i) = (ops'' ++ ((0x01000000 .|. i'):ops), i'') 
  where (ops', i') = genOPs ins ([], i+1)
        (ops'', i'') = genOPs c (ops', i')        
genOPs []                (ops@(0x40CE06C0:_),i) = (ops, i) -- no need for End after RetC 
genOPs []                (ops@(0x01430000:_),i) = (ops, i) -- no need for End after Ret
genOPs []                (ops@(0x404C02A0:_),i) = (ops, i) -- no need for End after AppT
genOPs []                (ops,i) = (0x7FFFFFFF:ops, i+1) 







test = (APP [(LAM "f" (APP [(VAR "f"), (CONST 10)])), (APP [(LAM "y" (LAM "x" (PRIM_2 Add (VAR "x") (VAR "y")))), (CONST 5)])])   
     

test2 = (APP [(LAM "f" (LAM "x" (APP[(VAR "f"), (APP [(VAR "f"), (VAR "x")])]))), (LAM "y" (PRIM_2 Add (VAR "y") (VAR "y"))), (CONST 5)])



test3 = (LET "x" (CONST 25) 
        (LET "y" (CONST 5)
            (PRIM_2 Add (VAR "x") (VAR "y"))))

test4 = (LET "x" (CONST 0) 
        (LET "y" (CONST 5)
            (IF (VAR "x") (CONST 1) (CONST 2))))

-- test5 = (LETREC "fact" 
--             (LAM "x" 
--                ()))

test6 = (LETREC "cntdn" 
            (LAM "x" 
               (IF (VAR "x")
                  (APP [(VAR "cntdn"), (PRIM_2 Sub (VAR "x") (CONST 1))])
                  (CONST 0)))
            (APP [(VAR "cntdn"), (CONST 5)]))



-- (def clos
--     (let x 5
--     (let y 25
--         (lam z 
--             (- y (+ z x))))))








----------------------------------------------------------------------------------------------
-- Evaluator ---------------------------------------------------------------------------------

data VAL = VNum Word32
         | VClos CLO
         | VMark 
         deriving (Eq, Show)

type ENV = [VAL]

type CLO = ([INST], ENV)

--          -code- -acc--env- -arg-  -ret-
type ZAM = ([INST], VAL, ENV, [VAL], [CLO])     

-- mkZam :: Expr -> ZAM
-- mkZam l = (c, VMark, [], [], []) where c = compile $ exprToDB l


-- eval :: ZAM -> ZAM                                        
-- eval ([], a, e, s, r) = ([], a, e, s, r) -- End
-- eval st = eval (step st)

-- step :: ZAM -> ZAM
-- step ((Access i):c0, a, e, s, r) = (c0, e!!((fromIntegral i)), e, s, r)
-- step (Apply:c0, a@(VClos (c1, e1)), e0, v:s, r) = (c1, a, v:e1, s, (c0, e0):r) 
-- step (Appterm:c0, a@(VClos (c1, e1)), e0, v:s, r) = (c1, a, v:e1, s, r)     
-- step (Push:c0, a, e, s, r) = (c0, a, e, a:s, r)
-- step (Pushmark:c0, a, e, s, r) = (c0, a, e, VMark:s, r)
-- step ((Cur c1):c0, a, e, s, r) = (c0, VClos (c1, e), e, s, r)
-- step ((If c1):c0, a@(VNum 0), e, s, r) = (c0, a, e, s, r)
-- step ((If c1):c0, a, e, s, r) = (c1, a, e, s, r)
-- step (Grab:c0, a, e0, VMark:s, (c1,e1):r) = (c1, VClos (c0, e0), e1, s, r)
-- step (Grab:c, a, e, v:s, r) = (c, a, v:e, s, r)
-- step (RetC:c0, a, e0, VMark:s, (c1,e1):r) = (c1, a, e1, s, r)
-- step (RetC:c0, a@(VClos (c1,e1)), e0, v:s, r) = (c1, a, v:e1, s, r)
-- step (Let:c0, a, e, s, r) = (c0, a, a:e, s, r)  
-- step (EndLet:c0, a, v:e, s, r) = (c0, a, e, s, r) 
-- step (Dummy:c0, a, e, s, r) = (c0, a, VMark:e, s, r) 
-- step (Update:c0, a, v:e, s, r) = (c0, a, a:e, s, r) 
-- step ((Prim_2 i):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a+x), e, s, r)
-- step ((Prim Sub):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a-x), e, s, r)
-- step ((Prim And):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a.&.x), e, s, r)
-- step ((Prim Or):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a.|.x), e, s, r) 
-- step ((Const x):c0, a, e, s, r) = (c0, VNum x, e, s, r)                           

-- sim' :: ZAM -> IO()
-- sim' x@([], _,_,_,_) = do RetC()
-- sim' x = do
--    let y = step x
--    pretty y
--    sim' y        

-- sim :: Expr -> IO()
-- sim l = do
--    let z = mkZam l
--    pretty z
--    sim' z

-- pretty :: ZAM -> IO()
-- pretty (c, a, e, s, r) = do
--   putStr "Code: "
--   print c
--   putStr "A: "
--   print a
--   putStr "Env: "
--   print e
--   putStr "argS: "
--   print s
--   putStr "retS: "
--   print r
--   print "--------------------------"
