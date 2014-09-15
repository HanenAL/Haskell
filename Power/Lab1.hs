-- Assignment 1: The function takes y + 1 steps.

power :: Integer -> Integer -> Integer
power x y | y < 0 = error "power: negative argument"
power x 0 = 1
power x y = x * power x (y-1)

-- Assignment 2: Creates a list of y elements 'x' and calculates the product of all elements in the list.
power1 :: Integer -> Integer -> Integer
power1 x 0 = 1
power1 x y  = product(replicate (fromInteger(y)) (fromInteger(x)))

-- Assignment 3: If y is even it takes x^2 and halves the y value and loops until y = 1 because then the program stops. If y is odd it takes x and multiplies it with what the result would be if y was y-1  --> (RUNS THE EVEN FUNCTION).
power2 :: Integer -> Integer -> Integer
power2 x 0 = 1
power2 x y | y<0 = error "Do not use negative exponents!"
power2 x y | even (y) = power2 (x*x) (y `div` 2)
power2 x y | odd (y)  = x * (power2 x (y-1))

-- Assignment 4:

-- Test Cases: We test one case where y = odd, one case with y = even and y = 0. Y (index) is not defined for negative numbers in any function. 

listX = [(-6), 0, 9, 15]
listY = [0, 1, 2, 3]

-- Compares the values that two of the power functions returns.
-- If they are equal to eachother we return True, otherwise False
comparePower1 :: Integer -> Integer -> Bool
comparePower1 x y | power x y == power1 x y = True
comparePower1 x y | otherwise = False

comparePower2 :: Integer -> Integer -> Bool
comparePower2 x y | power x y == power2 x y = True
comparePower2 x y | otherwise = False

-- Takes values from the two lists we defined at line 24 and 25,
-- and uses them in the compare functions
test1 = and [(comparePower1 x y) == (comparePower2 x y) 
    | x <- listX, y <- listY]
