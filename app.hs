module App where
import System.Environment 
import Plaster

main = do
    contents <- readFile "example1.txt"
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
        if (length solved) > 0 then
        	print (fromArrayToPlaster (solved !! 0) size)
        else
        	print "Brak rozwiazan"
        return plaster
    else error "Niepoprawne wejście"
    
    