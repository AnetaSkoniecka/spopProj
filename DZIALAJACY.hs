import Data.List
import Data.Maybe



--zamienia element o wskazanym indexie. jezeli ix = -1 to nie zamienia nic
replaceNth n newVal (x:xs)
    | n == -1 = x:xs
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs
    


--ll : lista list
solve ll = if checkIfCanFinish ll then ll  --lista nie zawiera juz list, ktore maja zara/kropki - zwroc prawdilowe listy
          else concatMap solve' ll

solve' l = solve (replaceFirstZero l)

replaceFirstZero l = [replaceNth n x l| x<-[1..10], 1==1] --do dopisania warunek odrzucajacy bledne oraz x - lsita elementow ktora ma byc wpisana zmaiast zera
    where n = if isNothing (elemIndex 0 l) then -1
              else fromJust (elemIndex 0 l)

              
checkIfCanFinish ll = length (filter (\z -> isJust (elemIndex 0 z)) ll) ==0