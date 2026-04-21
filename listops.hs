{- HLINT ignore "Use head" -}
{- HLINT ignore "Used otherwise as a pattern" -}
import Data.List (sort)
main :: IO () 
main = do
    listActions []
listActions :: [Int] -> IO ()
listActions xs = do
    cmd<- getLine
    let l = words cmd
    case l!!0 of
        "add" -> do
            let x =read (l!!1)
            let lst=x:xs
            putStrLn $ "Current List=" ++ show lst
            listActions lst
        "del" -> do
            let newlst=tail xs
            putStrLn $ "Current List=" ++ show newlst
            listActions newlst
        "srt" -> do
            let lst= sort xs
            putStrLn $ "Current List=" ++ show lst
            listActions lst
        "rev" -> do
            let lst= reverse xs
            putStrLn $ "Current List=" ++ show lst
            listActions lst
        "bye" -> do
            putStrLn "Exiting! Good bye!"
        otherwise -> do
            putStrLn "Invalid operation!"