module Machines
    where

import Data.Word
import Data.List
import Data.Bits
import System.IO
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B 



data Lambda = CONST Word32
            | ADD Lambda Lambda 
            | AND Lambda Lambda 
            | OR Lambda Lambda           
            | APP [Lambda]
            | ABST String Lambda
            | VAR String
            deriving (Eq, Show)

data DBLambda = DB_CONST Word32
           | DB_ADD DBLambda DBLambda
           | DB_AND DBLambda DBLambda
           | DB_OR DBLambda DBLambda          
           | DB_APP [DBLambda]
           | DB_ABST DBLambda
           | DB_VAR String
           | DB_I Word32
           deriving (Eq, Show)

data SF a = FF | SS a
    deriving (Eq, Show)


lambda_2_db :: Lambda -> DBLambda
lambda_2_db l = db_translate [] l where
    db_translate env (CONST n) = DB_CONST n
    db_translate env (ADD x y) = DB_ADD x' y' where
        x' = db_translate env x
        y' = db_translate env y
    db_translate env (AND x y) = DB_AND x' y' where
        x' = db_translate env x
        y' = db_translate env y
    db_translate env (OR x y) = DB_OR x' y' where
        x' = db_translate env x
        y' = db_translate env y        
    db_translate env (ABST v l) = DB_ABST l' where
        l' = db_translate (v:env) l
    db_translate env (APP ls) = DB_APP ls' where
        ls' = map (db_translate env) ls        
    db_translate env (VAR v)
        = case (find v env) of
            SS n -> DB_I n
            FF -> DB_VAR v
        where 
            find v [] = FF
            find v (v':rest) | v == v' = SS 0
                             | otherwise = case find v rest of
                                            FF -> FF
                                            SS n -> SS (n+1)





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
          | Return
          ------------ Abstraction
          | Prim PINST
          | Const Word32
          deriving (Eq, Show)

data PINST = Add              
           | And
           | Or       
           deriving (Eq, Show)

data VAL = VNum Word32
         | VClos CLO
         | VMark 
         deriving (Eq, Show)

type ENV = [VAL]

type CLO = ([INST], ENV)

--          -code- -acc--env- -arg-  -ret-
type ZAM = ([INST], VAL, ENV, [VAL], [CLO])     

mkZam :: Lambda -> ZAM
mkZam l = (c, VMark, [], [], []) where c = compile $ lambda_2_db l


compile :: DBLambda -> [INST]
compile (DB_CONST k) = [Const k]
compile (DB_I n) = [Access n]
compile (DB_APP dbls) = [Pushmark] ++ dbls' ++ [Apply] 
                                where dbls' = intercalate [Push] $ map compile $ reverse dbls                                      
compile (DB_ABST dbl) = [Cur $ compile dbl ++ [Return]]  
compile (DB_ADD x y) = (compile x) ++ Push:(compile y) ++ [Prim Add]
compile (DB_AND x y) = (compile x) ++ Push:(compile y) ++ [Prim And]
compile (DB_OR x y) = (compile x) ++ Push:(compile y) ++ [Prim Or]

compileT :: DBLambda -> [INST]
compileT (DB_ABST dbl) = Grab:(compile dbl)
compileT (DB_APP dbls) = dbls' ++ [Appterm] 
                                where dbls' = intercalate [Push] $ map compile $ reverse dbls

eval :: ZAM -> ZAM                                        
eval ([], a, e, s, r) = ([], a, e, s, r) -- End
eval st = eval (step st)

step :: ZAM -> ZAM
step ((Access i):c0, a, e, s, r) = (c0, e!!((fromIntegral i)-1), e, s, r)
step (Apply:c0, a@(VClos (c1, e1)), e0, v:s, r) = (c1, a, v:e1, s, (c0, e0):r) 
step (Appterm:c0, a@(VClos (c1, e1)), e0, v:s, r) = (c1, a, v:e1, s, r)     
step (Push:c0, a, e, s, r) = (c0, a, e, a:s, r)
step (Pushmark:c0, a, e, s, r) = (c0, a, e, VMark:s, r)
step ((Cur c1):c0, a, e, s, r) = (c0, VClos (c1, e), e, s, r)
step (Grab:c0, a, e0, VMark:s, (c1,e1):r) = (c1, VClos (c0, e0), e1, s, r)
step (Grab:c, a, e, v:s, r) = (c, a, v:e, s, r)
step (Return:c0, a, e0, VMark:s, (c1,e1):r) = (c1, a, e1, s, r)
step (Return:c0, a@(VClos (c1,e1)), e0, v:s, r) = (c1, a, v:e1, s, r)  
step ((Prim Add):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a+x), e, s, r)
step ((Prim And):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a.&.x), e, s, r)
step ((Prim Or):c0, (VNum a), e, (VNum x):s, r) = (c0, VNum (a.|.x), e, s, r) 
step ((Const x):c0, a, e, s, r) = (c0, VNum x, e, s, r)                           


test = (APP [(ABST "f" (APP [(VAR "f"), (CONST 10)])), (APP [(ABST "y" (ABST "x" (ADD (VAR "x") (VAR "y")))), (CONST 5)])])



compileOut :: Lambda -> IO()
compileOut lam = do
  B.writeFile "lam.bin" bs 
    where bs        = encode $ map byteSwap32 $ reverse ops
          c         = compile $ lambda_2_db lam
          (ops,len) = genOPs c ([],0)


genOPs :: [INST] -> ([Word32], Word32) -> ([Word32], Word32)
genOPs ((Access db):ins) (ops,i) = genOPs ins ((0x24000000 .|. (db .&. 0x0000001F)):ops, i+1)
genOPs (Appterm:ins)     (ops,i) = genOPs ins (0x204C02A0:ops, i+1)
genOPs (Apply:ins)       (ops,i) = genOPs ins (0x205D02A0:ops, i+1)    
genOPs (Push:ins)        (ops,i) = genOPs ins (0x22040000:ops, i+1)    
genOPs (Pushmark:ins)    (ops,i) = genOPs ins (0x3E04F800:ops, i+1)                               
genOPs (Grab:ins)        (ops,i) = genOPs ins (0x288E04C0:ops, i+1)  
genOPs (Return:ins)      (ops,i) = genOPs ins (0x20CE06C0:ops, i+1) 
genOPs ((Const x):ins)   (ops,i) = genOPs ins ((0x80000000 .|. x):ops, i+1)  
genOPs ((Prim Add):ins)  (ops,i) = genOPs ins (0x3C0C1100:ops, i+1) 
genOPs ((Cur c):ins)     (ops,i) = (ops'' ++ ((0x10000000 .|. i'):ops), i'') 
  where (ops', i') = genOPs ins ([], i+1)
        (ops'', i'') = genOPs c (ops', i')
genOPs []                (ops@(0x20CE06C0:_),i) = (ops, i) 
genOPs []                (ops,i) = (0x60000000:ops, i+1) 


sim :: ZAM -> IO()
sim x@([], _,_,_,_) = do return()
sim x = do
   let y = step x
   print y
   sim y        


main = do
   let z = mkZam test
   sim z