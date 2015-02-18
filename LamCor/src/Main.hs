
module Main where

import            LamCor

main::IO()
main = do 
   src <- readFile "tst.src"
   let ins = compileSrc src   
   writeFile "lam.S" ins


