module App where
import System.Environment 
import Plaster
import Data.Maybe

main = do
    contents <- readFile "example1.txt"
    if checkInput contents then do
        let rowLines = lines contents
        let plaster = parsePlaster rowLines
        let solvedPlaster = solveWithPrepare plaster
        return (plaster, solvedPlaster)
    else error "Niepoprawne wejście"
    
    