addUs :: Int -> Int -> Int
addUs a b = a+b 

multiply :: Int -> Int -> Int -> Int 

multiplyUs :: Int -> Int -> Int 

multiply count num sum | count ==0 = sum
| otherwise = multiply (count-1) num (addUs num sum) 

multiplyUs | a>0 && b>0 = multiply a b 0
| a<0 && b>0 = -(multiply (-a) b 0)
| a>0 && b<0 = -(multiply a (-b) 0)
| a>0 && b>0 = multiply (-a) (-b) 0
| otherwise =0





isPrime x y | y==1 = True
            | x==1 || mod x y == 0 = False
            | otherwise = isPrime x (y-1) 

prime x = isPrime x (x-1)


countDigit x y | x==0=y
               | otherwise= countDigit (quot x 10) (y+1) 

checkIt y x s | x==0 = s
            | otherwise= checkIt y (quot x 10) (s + (mod x 10)^(countDigit y 0)) 

armstrong y | (checkIt y y 0) ==y = True
            | otherwise = False
