import Data.List
import Data.Maybe



--ll : lista list
solve ll = if checkIfCanFinish ll then ll  --lista nie zawiera juz list, ktore maja zara/kropki - zwroc prawdilowe listy
           else concatMap solve' ll
       where checkIfCanFinish ll = length (filter (\z -> isJust (elemIndex 0 z)) ll) ==0
    
solve' l = solve (replaceFirstZero l)

replaceFirstZero l = filter isSorted [replaceNth n x l| x <- getPossibleGuesses l] 
    where n = if isNothing (elemIndex 0 l) then -1
              else fromJust (elemIndex 0 l)

--zamienia element o wskazanym indexie. jezeli ix = -1 to nie zamienia nic
replaceNth n newVal (x:xs)
    | n == -1 = x:xs
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

    
--zamiast tego powinna byc funkcja, ktora sprawdza czy plansza podana jako lista jest prawidlowa
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = (x <= y || y == 0) && isSorted (y:xs)


--zamaist tego powinna byc funkcja, ktora zwraca dopuszcalnych sasiadow
getPossibleGuesses l = [1..10]

