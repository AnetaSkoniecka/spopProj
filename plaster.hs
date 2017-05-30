module Plaster where
import System.Environment 
import Data.Maybe
import Data.List
import Data.Function (on)

----------------------
-- struktury danych --

data Plaster = Plaster { rows::[Row], size::Int }
data Row = Row { fields::[Field], h::Int }
data Field = Field { fieldType::FieldType, x::Int, y::Int }
data FieldType = Empty| A | B | C | D | E | F | G  deriving (Eq, Enum, Show, Ord)

--------------
-- algorytm --

--ll : lista list
solve :: [[Field]] -> Int -> [[Field]]
solve [] _ = []
solve (ll @ (x:xs)) size = if checkIfCanFinish ll then [x]  --lista nie zawiera juz list, ktore maja zara/kropki - zwroc prawdilowe listy
           else concatMap solve' [(l,size) | l <- ll]

checkIfCanFinish [] = False
checkIfCanFinish (l:ls) = checkIfCanFinish' l || checkIfCanFinish ls
checkIfCanFinish' [] = True
checkIfCanFinish' ((f @ (Field ft x y)):fs) = (ft /= Empty) && (checkIfCanFinish' fs)
    
solve' (l,size) = solve (replaceFirstZero l size) size

replaceFirstZero l size = map (\(a,b) -> a) (filter isListCorrect [((replaceNth n newVal l), size)| newVal <- getPossibleGuesses l n]) 
    where n = firstEmptyIndex l

firstEmptyIndex l = firstEmptyIndex' l 0
firstEmptyIndex' [] _ = -1
firstEmptyIndex' ((f @ (Field ft x y)):fs) i | ft == Empty = i
    | otherwise = firstEmptyIndex' fs (i+1)

--zamienia element o wskazanym indexie. jezeli ix = -1 to nie zamienia nic
replaceNth n newVal ((f @ (Field ft x y)):fs)
    | n == -1 = f:fs
    | n == 0 = (Field newVal x y):fs
    | otherwise = f:replaceNth (n-1) newVal fs

    
--zamiast tego powinna byc funkcja, ktora sprawdza czy plansza podana jako lista jest prawidlowa
isListCorrect (l,s) = isListCorrect' l l size
isListCorrect' _ [] size = True
isListCorrect' l (f:fs) size = (isElemCorrect l f) && isListCorrect' l fs size
isElemCorrect l f = do
    let fields = f:(getNeightbours l f)
    length ([ (atype,btype) | (Field atype ax ay) <- fields, (Field btype bx by) <- fields, (((ax /= bx) || (ay /= by)) && (areTypesEqual atype btype))]) == 0

areTypesEqual ftype ntype | (ftype == Empty && ntype == Empty) = False
    | otherwise = (ftype == ntype)  

getNeightbours l (Field t x y) | y `mod` 2 == 0 = [ el | el @ (Field et ex ey) <- l, (nx, ny) <- [(x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x,y+1), (x+1,y+1)], ((ex == nx) && (ey == ny))]
    | otherwise = [ el | el @ (Field et ex ey) <- l, (nx, ny) <- [(x-1,y-1), (x, y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1)], ((ex == nx) && (ey == ny))]


--zamaist tego powinna byc funkcja, ktora zwraca dopuszcalnych sasiadow
getPossibleGuesses l n = do
    let f = l !! n
    [ c | c <- [A, B, C, D, E, F, G], (Field t x y) <- f:(getNeightbours l f), t /= c]







--------------------------
-- operacje na plastrze --

getAllFields (Plaster rows size) = concat [ y | y <- map (\(Row fields _) -> fields ) rows] 

   
getField :: Plaster -> Int -> Int -> Maybe Field
getField (Plaster rows size) x y = if y < 0 || y >= (length rows)then Nothing 
    else getField1 (rows!!y) x

getField1 :: Row -> Int-> Maybe Field
getField1 (Row fields _) x = if x < 0 || x >= (length fields) then Nothing
    else Just (fields!!x) 
    
fieldName (Field fieldType _ _) = fromEnum fieldType
sortFields :: [Field] -> [Field]
sortFields = sortBy (compare `on` fieldName)
  
--getNeighbours _ Nothing = error "szukasz sasiada nieistniejacego pola"
getNeighbours p  (Field _ x y) = if y `mod` 2 == 0 then sortFields (getNeighboursEven p x y)
    else sortFields (getNeighboursOdd p x y)
   
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
    
--zwraca pary (pole, liczba niepustych sasiadow)
getNotEmptyNeighboursCount p = [(x, length notEmptyNeighbours)| x <- getAllFields p, let notEmptyNeighbours = filter (\(Field ft _ _) -> ft /= Empty) (getNeighbours p x )]   

--zwraca pole ktore ma najwiecej wypelnionych sasiadow na planszy, ale nie ma ich wypelnionych wszystkich: czyli po prostu ma najmniej kropek w stosunku do liczby wypelnionych
--to chyba moze sypnac wyjatek!
getMostNeigboured p = fst (reverse (sortBy (compare `on` snd) (filter (\(field, cnt) -> cnt < length(getNeighbours p field)) (getNotEmptyNeighboursCount p))) !! 0)

isFieldCorrect :: Plaster -> Field -> Bool
isFieldCorrect plaster field @ (Field ftype x y) = do
    let neighbours = getNeighbours plaster field
    let centerDiffrent = (length ([ ntype | (Field ntype nx ny) <- neighbours,  (areFieldsEqual ftype ntype)])) == 0
    let neighboursDiffrent = (length ([ (atype,btype) | (Field atype ax ay) <- neighbours, (Field btype bx by) <- neighbours, (((ax /= bx) || (ay /= by)) && (areFieldsEqual atype btype))])) == 0
    centerDiffrent && neighboursDiffrent

areFieldsEqual ftype ntype | (ftype == Empty && ntype == Empty) = False
    | otherwise = (ftype == ntype)

isPlasterCorrect :: Plaster -> Bool
isPlasterCorrect p = (length ([ f | f <- (getAllFields p), (not (isFieldCorrect p f))])) == 0
 
-------------------------
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
    show (Row fields h) | h `mod` 2 == 0 = "  " ++ showFields fields
                | otherwise = showFields fields

showRows :: [Row] -> String
showRows [] = []
showRows (r:rs) = show r ++ "\n" ++ showRows rs

instance Show Plaster where
    show (Plaster rows size) = showRows rows

---------------------------
-- parsowanie wewnętrzne --

plasterToArray :: Plaster -> [Char]
plasterToArray (Plaster [] a) = []
plasterToArray (Plaster (r:rs) a) = (rowToArray r) ++ (plasterToArray (Plaster rs a))

rowToArray :: Row -> [Char]
rowToArray (Row [] _) = []
rowToArray (Row (f:fs) h) = (fieldToChar f) : (rowToArray (Row fs h))

fieldToChar :: Field -> Char
fieldToChar (Field fieldType _ _) | fieldType == A = 'A'
    | fieldType == B = 'B'
    | fieldType == C = 'C'
    | fieldType == D = 'D'
    | fieldType == E = 'E'
    | fieldType == F = 'F'
    | fieldType == G = 'G'
    | fieldType == Empty = '.'

fromArrayToStrings [] _ _ = []
fromArrayToStrings array rIndex size | ((rIndex `mod` 2) == 0) = (take (size-1) array) : (fromArrayToStrings (drop (size-1) array) (rIndex+1) size)
    |  otherwise = (take (size) array) : (fromArrayToStrings (drop (size) array)  (rIndex+1) size)
 
fromArrayToPlaster array size = parsePlaster (fromArrayToStrings array 0 size)

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
parsePlaster lines = Plaster (parseRows lines 0) (length lines)

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
