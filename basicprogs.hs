--smallest divisor of n >1
smallestDiv n= divsearch n 2
divsearch i m
  |mod i m==0 = m
  |otherwise = divsearch i (m+1)
--	defining divsearch as auxillary
smallestDiv1 n =
 let divsearch i
       |mod n i==0 = i
       |otherwise = divsearch (i+1)
 in divsearch 2


--length of positive integer
lengthint::Integer -> Integer
lengthint n
 |n<0= error "negative number"
 |n<10= 1
 |otherwise= 1+lengthint(div n 10)

--reversing a number
revint::Integer->Integer
revint n 
 |n<0 = error "negative"
 |n <10 = n
 |otherwise = 
              let pow i m 
                   |m==0 = 1
                   |otherwise = i*pow i (m-1)
              in revint(div n 10) + (mod n 10)*pow 10 (lengthint n-1)

--(or) using accumulator
revint1 n= revacc 0 n where
 revacc i m 
  |m==0 = i
  |otherwise= revacc ((10*i)+ mod m 10) (div m 10)

--list length
listlen::[Int]->Int
listlen l = if null l 
    then 0 
    else let {x:xs=l} 
         in listlen xs + 1
