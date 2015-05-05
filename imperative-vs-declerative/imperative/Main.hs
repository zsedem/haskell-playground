module Main where
import Control.Monad(forever)
import System.IO(withFile, IOMode(..), hGetLine)
import System.IO.Error(tryIOError)
import Data.IORef

(+=) :: Num a => IORef a -> a -> IO ()
a += b = modifyIORef a (+b)

main :: IO ()
main = do
    sumRef <- newIORef (0::Int)
    withFile "numbers.txt" ReadMode $ \file -> tryIOError $ forever $ do
        line <- hGetLine file
        sumRef += read line
    readIORef sumRef >>= print
