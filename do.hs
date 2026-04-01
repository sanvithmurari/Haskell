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
        printFive 1 str= putStrLn str
        printFive n str = do
            putStrLn str
            printFive (n-1) str
    

