{-# LANGUAGE BlockArguments #-}
main1 = let {
    fact n
    |n==1=1
    |otherwise = n*fact (n- 1)
} in do
    putStrLn $ "Factorial of 2 is: " ++ show (fact 2)

main2= do{
    action1; action2;
}where
    action1=putStrLn "Hello"
    action2=putStrLn "What's up?"

main3=do
    putStr "Enter your Name: \n"
    name <- getLine
    print ("Name= " ++ name)

--Recursion in a do block
mainRec=do
    putStr "Enter line to be printed 5 times using Recursion"
    input <- getLine
    printFive 5 input
    where
        printFive 1 str= putStrLn str --OR printFive 0 str = return ()
        printFive n str = do
            putStrLn str
            printFive (n-1) str

--Repeat an IO action n times 
nTimes :: Int -> IO () -> IO()
nTimes 0 a = return ()
nTimes n a = do
    a
    nTimes (n-1) a

main4= nTimes 3 do{
    putStr "Enter a Line: ";
    str <- getLine;
    putStrLn str;
}

main5 = do{
    putStrLn "Enter a Line: ";
    line <- getLine;
    nTimes 5 (putStrLn line);
}

