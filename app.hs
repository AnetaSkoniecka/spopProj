module App where
import System.Environment 
import Plaster

main = do
	contents <- readFile "example.txt"
	if checkInput contents then do
		let rowLines = lines contents
		let plaster = parsePlaster rowLines
		return plaster
	else error "Niepoprawne wejście"