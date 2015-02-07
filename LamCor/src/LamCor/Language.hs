
module LamCor.Language
(Expr(..)
,POp(..) 
,SymbTable 
,compile
,compileT
,genOPs
,exprToDB
,tailCallOpt
,packOpt
,splitInt
)
  where

import Data.Word
import Data.List
import Data.Bits
import qualified Data.Map as M

type Symb = String
type SymbTable = M.Map Symb [Word8]

data Expr = Const Word32 
          | Def Symb Expr                      
          | App [Expr]
          | Lam Symb Expr
          | Let Symb Expr Expr
          | LetRec Symb Expr Expr
          | If Expr Expr Expr          
          | Var Symb
          | Get Symb
          | Set Symb Word32
          ------------------------------
          | Prim_1 POp Expr
          | Prim_2 POp Expr Expr 
          -- | Inst Word32        
          deriving (Eq, Show)


data DBExpr = DB_CONST Word32                             
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
            | DB_I Word8
            | DB_PRIM_1 POp DBExpr
            | DB_PRIM_2 POp DBExpr DBExpr
            | DB_INST [Word8]          
            deriving (Eq, Show)

data SF a = FF | SS a
    deriving (Eq, Show)


exprToDB :: SymbTable -> Expr -> DBExpr
exprToDB symt l = db_translate [] l where
       db_translate _ (Const n) = DB_CONST n  
       --db_translate _ (Inst x) = DB_INST x      
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
            FF -> case (M.lookup v symt) of
                Nothing   -> DB_INST [0,0,0]
                Just ins -> DB_INST ins
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
packOpt ((LDI x):PUSH:is) = (LDIP x):(packOpt is) 
packOpt ((ACC x):PUSH:is) = (ACCP x):(packOpt is) 
packOpt (RETC:RETC:is) = RETC:(packOpt is) 
packOpt (i:is) = i:(packOpt is)
packOpt [] = []


-- ZAM --

data Inst = NOP
          | LDI Word32
          | LDIP Word32
          | CLOS [Inst]
          | IF [Inst]
          | CALL Word32
          | JUMP Word32
          | RET
          | ACC Word8
          | ACCP Word8
          | APPT
          | APP
          | PUSH
          | MARK
          | GRAB
          | RETC
          | LET
          | ELET
          | TEMP
          | UPDT
          | END
          | GET Word32
          | SET Word32 Word32
          | PRIM POp          
          | INST [Word8]         
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
compile (DB_LAM dbl) = [CLOS $ compileT dbl ++ [RETC]] 
compile (DB_LET v e) = (compile v) ++ LET:(compile e) ++ [ELET]  
compile (DB_LETREC v e) = TEMP:(compile v) ++ UPDT:(compile e) ++ [ELET] 
compile (DB_IF b t e) = (compile b) ++ [IF ((compile t) ++ [RETC])]  ++ (compile e) ++ [RETC] 
compile (DB_CALL a) = [CALL a]
compile (DB_JUMP a) = [JUMP a]
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

splitInt :: Word32 -> [Word8]
splitInt a  
  | a' >= 0 && a' <= 0x000000FF                 = [b0] 
  | a' >= 0 && a' <= 0x0000FFFF                 = b1:[b0]
  | a' >= 0 && a' <= 0x00FFFFFF                 = b2:b1:[b0]
  | a' >= 0 && a' <= 0x7FFFFFFF                 = b3:b2:b1:[b0]
  -- | a' < 0 && (a .&. 0xFFFFFF00) == 0xFFFFFF00 = [b0]
  -- | a' < 0 && (a .&. 0xFFFF0000) == 0xFFFF0000 = b1:[b0]
  -- | a' < 0 && (a .&. 0xFF000000) == 0xFF000000 = b2:b1:[b0]
  | otherwise                                  = b3:b2:b1:[b0]
    where b0 = fromIntegral (a .&. 0x000000FF)  :: Word8
          b1 = fromIntegral $ shiftR (a .&. 0x0000FF00) 8 :: Word8
          b2 = fromIntegral $ shiftR (a .&. 0x00FF0000) 16 :: Word8  
          b3 = fromIntegral $ shiftR (a .&. 0xFF000000) 24 :: Word8  
          a' = fromIntegral a :: Int

splitAddr16 :: Word32 -> [Word8]
splitAddr16 a 
  | l == 0    = 0x00:[0x00]
  | l == 1    = 0x00:bs
  | otherwise = bs
    where bs = splitInt a
          l  = length bs            

-- align :: ([Word8], Word32) -> ([Word8], Word32)
-- align (ins,i)
--   | i .&. 0x00000003 /= 0 = align (0x00:ins,i+1)
--   | otherwise             = (ins,i)     


genOPs :: [Inst] -> ([Word8], Word32) -> ([Word8], Word32)
genOPs (NOP:ins)         (ops,i)  = genOPs ins (0x00:ops, i+1)
genOPs ((ACC db):ins)    (ops,i)  = genOPs ins (db:op:ops, i+2) where
  op = 0x20
genOPs ((ACCP db):ins)   (ops,i)  = genOPs ins (db:op:ops, i+2) where
  op = 0x24 
genOPs (APPT:ins)        (ops,i)  = genOPs ins (0x28:ops, i+1)
genOPs (APP:ins)         (ops,i)  = genOPs ins (0x2C:ops, i+1)    
genOPs (PUSH:ins)        (ops,i)  = genOPs ins (0x30:ops, i+1)    
genOPs (MARK:ins)        (ops,i)  = genOPs ins (0x34:ops, i+1)                               
genOPs (GRAB:ins)        (ops,i)  = genOPs ins (0x38:ops, i+1)
genOPs ((CALL a):ins)    (ops,i)  = genOPs ins (bs++(op:ops), i+1+l32) where
  bs   = splitInt a
  l    = fromIntegral (length bs) :: Word8
  l32  = fromIntegral (length bs) :: Word32
  op = 0x14 .|. (l - 1)
genOPs ((JUMP a):ins)    (ops,i)  = genOPs ins (bs++(op:ops), i+1+l32) where
  bs = splitInt a
  l    = fromIntegral (length bs) :: Word8
  l32  = fromIntegral (length bs) :: Word32
  op = 0x18 .|. (l - 1) 
genOPs (RET:ins)         (ops,i)  = genOPs ins (0x1C:ops, i+1)   
genOPs (RETC:ins)        (ops,i)  = genOPs ins (0x3C:ops, i+1) 
genOPs (LET:ins)         (ops,i)  = genOPs ins (0x40:ops, i+1)
genOPs (ELET:ins)        (ops,i)  = genOPs ins (0x44:ops, i+1)
genOPs (TEMP:ins)        (ops,i)  = genOPs ins (0x48:ops, i+1)
genOPs (UPDT:ins)        (ops,i)  = genOPs ins (0x4C:ops, i+1)
genOPs ((LDI x):ins)     (ops,i)  = genOPs ins (bs++(op:ops), i+1+l32) where
  bs = splitInt x
  l    = fromIntegral (length bs) :: Word8
  l32  = fromIntegral (length bs) :: Word32
  op = 0x04 .|. (l - 1)
genOPs ((LDIP x):ins)     (ops,i)  = genOPs ins (bs++(op:ops), i+1+l32) where 
  bs = splitInt x
  l    = fromIntegral (length bs) :: Word8
  l32  = fromIntegral (length bs) :: Word32
  op = 0x08 .|. (l - 1)  
genOPs ((PRIM Add):ins)  (ops,i)  = genOPs ins (0xA0:ops, i+1) 
genOPs ((PRIM Sub):ins)  (ops,i)  = genOPs ins (0xA4:ops, i+1)
genOPs ((PRIM Mul):ins)  (ops,i)  = genOPs ins (0xA8:ops, i+1)
genOPs ((PRIM And):ins)  (ops,i)  = genOPs ins (0xAC:ops, i+1)
genOPs ((PRIM Or):ins)   (ops,i)  = genOPs ins (0xB0:ops, i+1)
genOPs ((PRIM Not):ins)  (ops,i)  = genOPs ins (0xB4:ops, i+1)
genOPs ((PRIM Xor):ins)  (ops,i)  = genOPs ins (0xB8:ops, i+1)
genOPs ((PRIM Xnor):ins) (ops,i)  = genOPs ins (0xBC:ops, i+1)
genOPs ((PRIM Ashr):ins) (ops,i)  = genOPs ins (0xC0:ops, i+1)
genOPs ((PRIM Lshr):ins) (ops,i)  = genOPs ins (0xC4:ops, i+1)
genOPs ((PRIM Lshl):ins) (ops,i)  = genOPs ins (0xC8:ops, i+1)
genOPs ((PRIM Eq):ins)   (ops,i)  = genOPs ins (0xCC:ops, i+1)
genOPs ((PRIM Ne):ins)   (ops,i)  = genOPs ins (0xD0:ops, i+1)
genOPs ((PRIM Gt):ins)   (ops,i)  = genOPs ins (0xD4:ops, i+1)
genOPs ((PRIM Lt):ins)   (ops,i)  = genOPs ins (0xD8:ops, i+1)
genOPs ((PRIM Gte):ins)  (ops,i)  = genOPs ins (0xDC:ops, i+1)
genOPs ((PRIM Lte):ins)  (ops,i)  = genOPs ins (0xE0:ops, i+1)
genOPs ((PRIM Abv):ins)  (ops,i)  = genOPs ins (0xE4:ops, i+1)
genOPs ((PRIM Bel):ins)  (ops,i)  = genOPs ins (0xE8:ops, i+1)

genOPs ((INST xs):ins)    (ops,i) = genOPs ins (xs++ops, i+l) where
  l  = fromIntegral (length xs)

genOPs ((CLOS c):[])      (ops,i) = (ops'++addr++(op:ops), i') 
  where (ops', i')  = genOPs c (ops,  i+3)        
        addr         = splitAddr16 (i+3)
        op = 0x0C .|. 1        

genOPs ((CLOS c):ins)     (ops,i) = (ops''++addr++(op:ops), i'') 
  where (ops', i') = genOPs ins ([], i+3)
        (ops'', i'') = genOPs c (ops', i')        
        addr         = splitAddr16 i'
        op = 0x0C .|. 1

genOPs ((IF c):ins)     (ops,i) = (ops''++addr++(op:ops), i'') 
  where (ops', i')   = genOPs ins ([], i+3)
        (ops'', i'') = genOPs c (ops', i') 
        addr         = splitAddr16 i'        
        op = 0x10 .|. 1        
genOPs []                (ops@(0x3C:_),i) = (ops, i) -- no need for End after RETC 
genOPs []                (ops@(0x1C:_),i) = (ops, i) -- no need for End after Ret
genOPs []                (ops@(0x28:_),i) = (ops, i) -- no need for End after AppT
genOPs []                (ops,i) = (0x50:ops, i+1) 


























test = (App [(Lam "f" (App [(Var "f"), (Const 10)])), (App [(Lam "y" (Lam "x" (Prim_2 Add (Var "x") (Var "y")))), (Const 5)])])   
     

test2 = (App [(Lam "f" (Lam "x" (App[(Var "f"), (App [(Var "f"), (Var "x")])]))), (Lam "y" (Prim_2 Add (Var "y") (Var "y"))), (Const 5)])



test3 = (Let "x" (Const 25) 
        (Let "y" (Const 5)
            (Prim_2 Add (Var "x") (Var "y"))))

test4 = (Let "x" (Const 0) 
        (Let "y" (Const 5)
            (If (Var "x") (Const 1) (Const 2))))

-- test5 = (LetRec "fact" 
--             (Lam "x" 
--                ()))

test6 = (LetRec "cntdn" 
            (Lam "x" 
               (If (Var "x")
                  (App [(Var "cntdn"), (Prim_2 Sub (Var "x") (Const 1))])
                  (Const 0)))
            (App [(Var "cntdn"), (Const 5)]))



-- (def clos
--     (let x 5
--     (let y 25
--         (lam z 
--             (- y (+ z x))))))




