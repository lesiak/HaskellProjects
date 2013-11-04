module Main where 
---import System.Environment

import System.IO



main :: IO ()                
main = do
    
    
    putStrLn (show 1)
    ---hPutStr stdout "\n"
    hPutStr stdout (show (doubleMe 2))
    ---hPutStr stdout "\n"
    ---hPutStr stdout (show (is_zero_or_one 1))



doubleMe :: Num a => a -> a
doubleMe x = x + x 

doubleUs x y = doubleMe x + doubleMe y 

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]


addThree x y z = x + y + z  

factorial n = product [1..n]

greetings s = "Hello" ++ s

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!" 

factorial1 :: (Integral a) => a -> a  
factorial1 0 = 1  
factorial1 n = n * factorial (n - 1)  

firstT :: (a, b, c) -> a  
firstT (x, _, _) = x  

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"

bmiTell3 :: (RealFloat a) => a -> a -> String  
bmiTell3 weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname 

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

calcBmis2 :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  


head2 :: [a] -> a  
head2 [] = error "No head for empty lists!"  
head2 (x:_) = x  

head3 :: [a] -> a  
head3 xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               _:[] -> "a singleton list."   
                                               _ -> "a longer list."  

describeList2 :: [a] -> String  
describeList2 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [_] = "a singleton list."  
          what _ = "a longer list."  