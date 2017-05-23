module Plaster where
import System.Environment 
import Data.Maybe
import Data.Function (on)
import Data.List

--------------
-- algorytm --

--ll : lista list
solve :: [String] -> Int -> [String]
solve ll size = if checkIfCanFinish ll then ll  --lista nie zawiera juz list, ktore maja zara/kropki - zwroc prawdilowe listy
           else concatMap solve' [(l,size) | l <- ll]
       where checkIfCanFinish ll = length (filter (\z -> isJust (elemIndex '.' z)) ll) == 0
    
solve' (l,size) = solve (replaceFirstZero l size) size

replaceFirstZero l size = map (\(a,b) -> a) (filter isSorted [((replaceNth n x l), size)| x <- getPossibleGuesses l]) 
    where n = if isNothing (elemIndex '.' l) then -1
              else fromJust (elemIndex '.' l)

--zamienia element o wskazanym indexie. jezeli ix = -1 to nie zamienia nic
replaceNth n newVal (x:xs)
    | n == -1 = x:xs
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

    
--zamiast tego powinna byc funkcja, ktora sprawdza czy plansza podana jako lista jest prawidlowa
isSorted ([], size)       = True
isSorted ([x], size)      = True
isSorted ((array @ (x:y:xs)), size) = isPlasterCorrect (fromArrayToPlaster array size)

--zamaist tego powinna byc funkcja, ktora zwraca dopuszcalnych sasiadow
getPossibleGuesses l = ['A', 'B', 'C', 'D', 'E', 'F', 'G']

----------------------
-- struktury danych --

data Plaster = Plaster { rows::[Row], size::Int, backtrack::Bool }
data Row = Row { fields::[Field], h::Int }
data Field = Field { fieldType::FieldType, originalFieldType::FieldType, x::Int, y::Int }
data FieldType = Empty | A | B | C | D | E | F | G  deriving (Eq, Enum, Show, Ord)

-------------------------

--------------------------
-- parsowanie wewnetrze --

plasterToArray :: Plaster -> [Char]
plasterToArray (Plaster [] a b) = []
plasterToArray (Plaster (r:rs) a b) = (rowToArray r) ++ (plasterToArray (Plaster rs a b))

rowToArray :: Row -> [Char]
rowToArray (Row [] _) = []
rowToArray (Row (f:fs) h) = (fieldToChar f) : (rowToArray (Row fs h))

fieldToChar :: Field -> Char
fieldToChar (Field fieldType _ _ _) | fieldType == A = 'A'
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

--------------------
-- poprawność pól --

isFieldCorrect :: Plaster -> Field -> Bool
isFieldCorrect plaster field @ (Field ftype origftype x y) = (length ([ ntype | (Field ntype ontype nx ny) <- (getNeighbours plaster field),  ntype == ftype])) == 0

isPlasterCorrect :: Plaster -> Bool
isPlasterCorrect p = (length ([ f | f <- (getAllFields p), (not (isFieldCorrect p f))])) == 0

---------------------------------------------
-- wybieranie jednego pola i jego sąsiadów --

getAllFields :: Plaster -> [Field]
getAllFields (Plaster rows size backtrack) = concat [ y | y <- map (\(Row fields _) -> fields ) rows] 

getFieldNormal :: Plaster -> Int -> Int -> Field
getFieldNormal p x y = fromMaybe (Field Empty Empty 0 0) (getField p x y)
   
getField :: Plaster -> Int -> Int -> Maybe Field
getField (Plaster rows size backtrack) x y = if y < 0 || y >= (length rows)then Nothing 
    else getField1 (rows!!y) x

getField1 :: Row -> Int-> Maybe Field
getField1 (Row fields _) x = if x < 0 || x >= (length fields) then Nothing
    else Just (fields!!x) 
    
fieldName (Field fieldType origfieldtype _ _) = fromEnum fieldType
sortFields :: [Field] -> [Field]
sortFields = sortBy (compare `on` fieldName)
  
--getNeighbours _ Nothing = error "szukasz sasiada nieistniejacego pola"
getNeighbours p  (Field _ _ x y) = if y `mod` 2 == 0 then sortFields (getNeighboursEven p x y)
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
getNotEmptyNeighboursCount p = [(x, length notEmptyNeighbours)| x <- getAllFields p, let notEmptyNeighbours = filter (\(Field ft oft _ _) -> ft /= Empty) (getNeighbours p x )]   

--zwraca pole ktore ma najwiecej wypelnionych sasiadow na planszy, ale nie ma ich wypelnionych wszystkich: czyli po prostu ma najmniej kropek w stosunku do liczby wypelnionych
--to chyba moze sypnac wyjatek!
getMostNeigboured p = fst (reverse (sortBy (compare `on` snd) (filter (\(field, cnt) -> cnt < length(getNeighbours p field)) (getNotEmptyNeighboursCount p))) !! 0)


-------------------------
-- wyświetlanie danych --

instance Show Field where
    show (Field A _ _ _) = "A   "
    show (Field B _ _ _) = "B   "
    show (Field C _ _ _) = "C   "
    show (Field D _ _ _) = "D   "
    show (Field E _ _ _) = "E   "
    show (Field F _ _ _) = "F   "
    show (Field G _ _ _) = "G   "
    show (Field Empty _ _ _) = ".   "

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
    show (Plaster rows size backtrack) = showRows rows

-----------------------------------
-- parsowanie z pliku tekstowego --

parseFields :: String -> Int -> Int -> [Field]
parseFields [] _ _ = []
parseFields (s:sx) x y | s == 'A' = (Field A A x y) : parseFields sx (x + 1) y
    | s == 'B' = (Field B B x y) : parseFields sx (x + 1) y
    | s == 'C' = (Field C C x y) : parseFields sx (x + 1) y
    | s == 'D' = (Field D D x y) : parseFields sx (x + 1) y
    | s == 'E' = (Field E E x y) : parseFields sx (x + 1) y
    | s == 'F' = (Field F F x y) : parseFields sx (x + 1) y
    | s == 'G' = (Field G G x y) : parseFields sx (x + 1) y
    | otherwise = (Field Empty Empty x y) : parseFields sx (x + 1) y

parseRows :: [String] -> Int -> [Row]
parseRows [] _ = []
parseRows (l:ls) i = (Row (parseFields l 0 i) i) : parseRows ls (i + 1)

parsePlaster :: [String] -> Plaster
parsePlaster lines = Plaster (parseRows lines 0) (length lines) False

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