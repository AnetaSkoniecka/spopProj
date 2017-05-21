import Data.Array.IO  


type Board = IOArray Int Int



main = do
  arr <- newArray (1,5) 1
  writeArray arr 1 1
  writeArray arr 2 0
  writeArray arr 3 3
  writeArray arr 4 0
  writeArray arr 5 5
  
  solve arr 1
  
  {-
  a <- readArray abc 1
  print a
  a <- readArray abc 2
  print a
  a <- readArray abc 3
  print a
  a <- readArray abc 4
  print a
  a <- readArray abc 5
  print a
  -}




  
solve :: Board -> Int -> IO (Maybe Board)
solve a 5 = return (Just a)
solve a x = do 
            v <- readArray a x
            case v of
                0 -> getGuess a x >>= solve' a x
                _ ->  solve a (x+1)
            where solve' a x [] = return Nothing  
                  solve' a x (v:vs) = do writeArray a x v
                                         r <- solve a (x+1)
                                         writeArray a x 0  
                                         solve' a x vs 
                                 
                                 
    
getGuess a x = return [0..10]
    

    

    