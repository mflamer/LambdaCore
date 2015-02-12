
module Main where

--import            System.IO
import            LamCor
import            Data.List
import Data.Binary
import Data.Binary.Get()
import qualified Data.ByteString.Lazy as B 

main::IO()
main = do 
   src <- readFile "tst.src"
   let ins = compileSrc src
       bin = encode ins   
   B.writeFile "lam.bin" bin


-- compileOut :: String -> IO()
-- compileOut src = do
--   B.writeFile "lam.bin" bs 
--     where bs        = encode $ map byteSwap32 $ reverse ops
--           c         = compile $ exprToDB lam
--           (ops,len) = genOPs c ([],0)












--putStr $ foldl1 (++) (intersperse " | " (clex src))








