module App where
import System.Environment 
import Plaster

main = do
	contents <- readFile "example.txt"
	let rowLines = lines contents
	let plaster = parsePlaster rowLines
	return plaster