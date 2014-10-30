
module Main where

--import            System.IO
import            LamCor
import            Data.List

main::IO()
main = do 
   src <- readFile "lam.src"
   --putStr $ foldl1 (++) (intersperse " | " (clex src))
   putStr $ show $ parse src
























