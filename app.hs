module App where
import System.Environment 
import Plaster

main = do
    contents <- readFile "example.txt"
    if checkInput contents then do
        let rowLines = lines contents
        let plaster @ (Plaster rows size backtrack) = parsePlaster rowLines
        print plaster
        print size
        let array = plasterToArray plaster
        print array
        print (isPlasterCorrect plaster)
        let solved = solve [array] size
        print solved
        return plaster
    else error "Niepoprawne wejście"
    
    