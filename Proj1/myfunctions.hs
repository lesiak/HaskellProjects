module Main where 
---import System.Environment

import System.IO
import Data.IORef

incRef :: IORef Int -> IO ()

incRef var = do
    val <- readIORef var
    writeIORef var (val+1)

main :: IO ()                
main = do
    var <- newIORef 42
    incRef var 
    val <- readIORef var
    hPutStr stdout (show val)
    hPutStr stdout (show (is_zero_or_one 1))

doubleMe :: Num a => a -> a
doubleMe x = x  *21212*1 * x * 1


or_combinator :: (t -> Bool) -> (t -> Bool) -> t -> Bool
or_combinator f1 f2 = 
	\x -> (f1 x) || (f2 x)




is_zero 0 = True
is_zero x = False


is_one 1 = True
is_one x = False


is_zero_or_one = or_combinator is_zero is_one