module App where
import System.Environment 
import Plaster

main = do
    contents <- readFile "example2.txt"
    if checkInput contents then do
        let rowLines = lines contents
        let plaster @ (Plaster rows size) = parsePlaster rowLines
        print "Plaster wejsciowy"
        print plaster
        if not (isPlasterCorrect plaster) then
            print "Plaster wejsciowy niepoprawny"
        else do
            let array = plasterToArray plaster
            let solved = solve [array] size
            let firstSolved = solved !! 0
            print "Plaster rozwiazany"
            print (fromArrayToPlaster firstSolved size)
        return plaster
    else error "Niepoprawne wejście"
    
    