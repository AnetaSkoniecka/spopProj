module Plaster where
import System.Environment 

----------------------
-- struktury danych --

data Plaster = Plaster { rows::[Row], niewiem::Int }
data Row = Row { fields::[Field], y::Int }
data Field = A | B | C | D | E | F | G | Empty deriving (Eq)

-------------------------
-- wyswietlanie danych --

instance Show Field where
	show A = "A   "
	show B = "B   "
	show C = "C   "
	show D = "D   "
	show E = "E   "
	show F = "F   "
	show G = "G   "
	show Empty = ".   "

showFields :: [Field] -> String
showFields [] = []
showFields (f:fs) = show f ++ showFields fs

instance Show Row where
	show (Row fields y) | y `mod` 2 == 1 = "  " ++ showFields fields
				| otherwise = showFields fields

showRows :: [Row] -> String
showRows [] = []
showRows (r:rs) = show r ++ "\n" ++ showRows rs

instance Show Plaster where
	show (Plaster rows niewiem) = showRows rows

-----------------------------------
-- parsowanie z pliku tekstowego --

parseFields :: String -> [Field]
parseFields [] = []
parseFields (s:sx) | s == 'A' = A : parseFields sx
	| s == 'B' = B : parseFields sx
	| s == 'C' = C : parseFields sx
	| s == 'D' = D : parseFields sx
	| s == 'E' = E : parseFields sx
	| s == 'F' = F : parseFields sx
	| s == 'G' = G : parseFields sx
	| otherwise = Empty : parseFields sx

parseRows :: [String] -> Int -> [Row]
parseRows [] _ = []
parseRows (l:ls) i = (Row (parseFields l) i) : parseRows ls (i + 1)

parsePlaster :: [String] -> Plaster
parsePlaster lines = Plaster (parseRows lines 1) 8

----------------------------------------------
-- sprawdzanie poprawnosci pliku tekstowego --

-- Zasady:
-- 1. Wejście musi mieć >0 lini (ale chyba powinniśmy zrobić >3 - do obgadania)
-- 2. Liczba n wynosi najdłuższą linię wczytaną z pliku
-- 3. Kolejne wiersze mają mieć długość  (n-1), n, (n-1),..,(n-1) 
-- 4. Wszystkie litery w pliku muszą należeć do zbioru: {A,B,C,D,E,F,G,.}

checkInput :: String -> Bool
checkInput contents = do
	let rows = lines contents
	if length rows == 0 then False
	else do
		let n = maxRowsSize rows
		(checkSizes rows (n - 1) n) && (checkAllLetters rows)

maxRowsSize :: [String] -> Int
maxRowsSize [] = 0
maxRowsSize (r:rs) = max (length r) (maxRowsSize rs)

checkSizes :: [String] -> Int -> Int -> Bool
checkSizes [] _ _ = True
checkSizes (r:rs) rn n = do
	let correctRow = (length r == rn)
	if n - 1 == rn then do
		correctRow && checkSizes rs n n
	else do
		correctRow && checkSizes rs (n - 1) n

checkAllLetters :: [String] -> Bool
checkAllLetters [] = True
checkAllLetters (r:rs) = checkRowLetters r && checkAllLetters rs

checkRowLetters :: String -> Bool
checkRowLetters [] = True
checkRowLetters (r:rs) = isLetter r && checkRowLetters rs

isLetter :: Char -> Bool
isLetter l | (l == 'A' || l == 'B' || l == 'C' || l == 'D' || l == 'E' || l == 'F' || l == 'G' || l == '.') = True
	| otherwise = False
