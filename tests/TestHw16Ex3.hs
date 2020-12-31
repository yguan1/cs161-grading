module TestHw16Ex3 where

import System.Process

-- Export main
main = do
    callCommand "ghc -main-is Hw16Ex3 -i.. Hw16Ex3.hs"
    callCommand "./Hw16Ex3 < ../include/gettysburg.txt"
    
