module Plaster where
import System.Environment 
import Data.Maybe
----------------------
-- struktury danych --

data Plaster = Plaster { rows::[Row] }
data Row = Row { fields::[Field], h::Int }
data Field = Field { fieldType::FieldType, x::Int, y::Int }
data FieldType = A | B | C | D | E | F | G | Empty deriving (Eq)

-------------------------
getField :: Plaster -> Int -> Int -> Maybe Field
getField (Plaster rows) x y = if y < 0 || y > (length rows)then Nothing 
    else getField1 (rows!!y) x

getField1 :: Row -> Int-> Maybe Field
getField1 (Row fields _) x = if x < 0 || x > (length fields) then Nothing
    else Just (fields!!x) 
    

getNeighbours _ Nothing = error "szukasz sasiada nieistniejacego pola"
getNeighbours p (Just (Field _ x y)) = if y `mod` 2 == 0 then getNeighboursEven p x y
    else getNeighboursOdd p x y
    
getNeighboursEven p x y=    
    maybeToList (getField p (x) (y-1) ) ++
    maybeToList (getField p (x+1) (y-1) ) ++
    maybeToList (getField p (x-1) (y) ) ++
    maybeToList (getField p (x+1) (y) ) ++
    maybeToList (getField p (x) (y+1) ) ++
    maybeToList (getField p (x+1) (y+1) )
    
getNeighboursOdd p x y=    
    maybeToList (getField p (x-1) (y-1) ) ++
    maybeToList (getField p (x) (y-1) ) ++
    maybeToList (getField p (x-1) (y) ) ++
    maybeToList (getField p (x+1) (y) ) ++
    maybeToList (getField p (x-1) (y+1) ) ++
    maybeToList (getField p (x) (y+1) ) 

-- wyswietlanie danych --

instance Show Field where
    show (Field A _ _) = "A   "
    show (Field B _ _) = "B   "
    show (Field C _ _) = "C   "
    show (Field D _ _) = "D   "
    show (Field E _ _) = "E   "
    show (Field F _ _) = "F   "
    show (Field G _ _) = "G   "
    show (Field Empty _ _) = ".   "

showFields :: [Field] -> String
showFields [] = []
showFields (f:fs) = show f ++ showFields fs

instance Show Row where
    show (Row fields h) | h `mod` 2 == 1 = "  " ++ showFields fields
                | otherwise = showFields fields

showRows :: [Row] -> String
showRows [] = []
showRows (r:rs) = show r ++ "\n" ++ showRows rs

instance Show Plaster where
    show (Plaster rows) = showRows rows

-----------------------------------
-- parsowanie z pliku tekstowego --

parseFields :: String -> Int -> Int -> [Field]
parseFields [] _ _ = []
parseFields (s:sx) x y | s == 'A' = (Field A x y) : parseFields sx (x + 1) y
    | s == 'B' = (Field B x y) : parseFields sx (x + 1) y
    | s == 'C' = (Field C x y) : parseFields sx (x + 1) y
    | s == 'D' = (Field D x y) : parseFields sx (x + 1) y
    | s == 'E' = (Field E x y) : parseFields sx (x + 1) y
    | s == 'F' = (Field F x y) : parseFields sx (x + 1) y
    | s == 'G' = (Field G x y) : parseFields sx (x + 1) y
    | otherwise = (Field Empty x y) : parseFields sx (x + 1) y

parseRows :: [String] -> Int -> [Row]
parseRows [] _ = []
parseRows (l:ls) i = (Row (parseFields l 0 i) i) : parseRows ls (i + 1)

parsePlaster :: [String] -> Plaster
parsePlaster lines = Plaster (parseRows lines 0)

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
        (checkSizes rows (n - 1) n) && (checkAllFieldTypes rows)

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

checkAllFieldTypes :: [String] -> Bool
checkAllFieldTypes [] = True
checkAllFieldTypes (r:rs) = checkRowFieldTypes r && checkAllFieldTypes rs

checkRowFieldTypes :: String -> Bool
checkRowFieldTypes [] = True
checkRowFieldTypes (r:rs) = isFieldType r && checkRowFieldTypes rs

isFieldType :: Char -> Bool
isFieldType l | (l == 'A' || l == 'B' || l == 'C' || l == 'D' || l == 'E' || l == 'F' || l == 'G' || l == '.') = True
    | otherwise = False
