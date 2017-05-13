module Plaster where
import System.Environment 

data Plaster = Plaster { rows::[Row], niewiem::Int }
data Row = Row { fields::[Field], y::Int }
data Field = A | B | C | D | E | F | G | Empty deriving (Eq)

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

parseFields :: String -> [Field]
--parseFields _ = [A, B, Empty , C]
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
