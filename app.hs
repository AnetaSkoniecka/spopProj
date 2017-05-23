module App where
import System.Environment 
import Plaster

main = do
    contents <- readFile "example1.txt"
    if checkInput contents then do
        let rowLines = lines contents
        let plaster = parsePlaster rowLines
        print plaster
        print (plasterToArray plaster)
        return plaster
    else error "Niepoprawne wejście"
    
    