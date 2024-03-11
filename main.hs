-- These options are to ignore suggestions from the compiler/IDE
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant return" #-}
-- Import IO and Hex-conversion library 
import System.IO ()
import Numeric (showHex, showIntAtBase)




----------------------------
-- BITWISE FUNCTIONS
----------------------------

-- Binary to Decimal conversion
binary_to_numeric :: [Int] -> Int -> IO Int
binary_to_numeric [] acc = return acc -- Base case
binary_to_numeric (x : xs) acc = do
  let newAcc = acc * 2 + x
  binary_to_numeric xs newAcc -- Recursion

--  Decimal to Binary conversion
numeric_to_binary :: Int -> [Int] -> IO [Int]
numeric_to_binary number bin_list = do
  let bin = number `mod` 2
  let new_num = number `div` 2
  if length bin_list < 8
    then numeric_to_binary new_num (bin : bin_list)
    else return (take 8 bin_list)

-- Addition Function
binary_addition :: [Int] -> [Int] -> [Int] -> IO [Int]
binary_addition bin1 bin2 res_list = do
  let reversed_bin1 = reverse bin1
      reversed_bin2 = reverse bin2
      reversed_res_list = reverse res_list
  binary_additionHelper reversed_bin1 reversed_bin2 0 reversed_res_list
-- Addition Function Helper
binary_additionHelper :: [Int] -> [Int] -> Int -> [Int] -> IO [Int]
binary_additionHelper [] [] 0 res_list = return (takeLast8 res_list) -- Base case when both lists are empty and no carry
binary_additionHelper [] [] carry res_list = return (takeLast8 (carry : res_list)) -- Base case when both lists are empty but carry exists
binary_additionHelper (x : bin1) (y : bin2) carry res_list = do
  let res = x + y + carry
      (new_bit, new_carry) = if res >= 2 then (res `mod` 2, 1) else (res, 0)
      new_res_list = new_bit : res_list
  binary_additionHelper bin1 bin2 new_carry new_res_list

-- Subtraction Function
binary_subtraction :: [Int] -> [Int] -> [Int] -> IO [Int]
binary_subtraction bin1 bin2 res_list = do
  let reversed_bin1 = reverse bin1
      reversed_bin2 = reverse bin2
      reversed_res_list = reverse res_list
  binary_subtractionHelper reversed_bin1 reversed_bin2 0 reversed_res_list
--Subtraction Function Helper
binary_subtractionHelper :: [Int] -> [Int] -> Int -> [Int] -> IO [Int]
binary_subtractionHelper [] [] 0 res_list = return (take 8 res_list) -- Base case when both lists are empty and no carry
binary_subtractionHelper [] [] carry res_list = return (takeLast8 (carry : res_list)) -- Base case when both lists are empty but carry exists
binary_subtractionHelper (x : bin1) (y : bin2) carry res_list = do
  let res = x - y + carry
      (new_bit, new_carry) = if res == -1 then (1, -1) else if res <= -2 then (0, -1) else (res, 0)
      new_res_list = new_bit : res_list
  binary_subtractionHelper bin1 bin2 new_carry new_res_list

-- And Function
binary_and :: [Int] -> [Int] -> [Int] -> IO [Int]
binary_and [] [] res_list = return res_list
binary_and (x : xs) (y : ys) res_list = do
  if x == 1 && y == 1
    then binary_and xs ys (1 : res_list)
    else do binary_and xs ys (0 : res_list)

-- Or Function
binary_or :: [Int] -> [Int] -> [Int] -> IO [Int]
binary_or [] [] res_list = return res_list
binary_or (x : xs) (y : ys) res_list = do
  if x == 1 || y == 1
    then binary_or xs ys (1 : res_list)
    else do binary_or xs ys (0 : res_list)

-- Xor Function
binary_xor :: [Int] -> [Int] -> [Int] -> IO [Int]
binary_xor [] [] res_list = return res_list
binary_xor (x : xs) (y : ys) res_list = do
  if x + y == 1
    then binary_xor xs ys (1 : res_list)
    else do binary_xor xs ys (0 : res_list)

-- Not Function
binary_not :: [Int] -> [Int] -> IO [Int]
binary_not [] res_list = return res_list
binary_not (x : xs) res_list = do
  if x == 1
    then binary_not xs (0 : res_list)
    else do binary_not xs (1 : res_list)

-- Last 8: We embed this in our addition/subtraction function to make sure there is no overflow
takeLast8 :: [Int] -> [Int]
takeLast8 list
  | length list > 8 = drop (length list - 8) list
  | otherwise = list




----------------------------
-- Arithmetic Operations
----------------------------

-- Select function for user input
artithmetic_operations :: String -> Int -> Int -> IO Int

artithmetic_operations "ADD" num1 num2 = do
  -- The ADD operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  bin2 <- numeric_to_binary num2 [] -- Convert to binary
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ show num1)
  putStrLn (" ADD " ++ show bin2 ++ " = " ++ show num2)
  binary_result <- binary_addition bin1 bin2 []
  putStrLn "     ---------------"
  numeric_result <- binary_to_numeric
   binary_result 0
  putStrLn ("     " ++ show binary_result)
  putStrLn ""
  return numeric_result

artithmetic_operations "SUB" num1 num2 = do
  -- The SUB operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  bin2 <- numeric_to_binary num2 [] -- Convert to binary
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ show num1)
  putStrLn (" SUB " ++ show bin2 ++ " = " ++ show num2)
  binary_result <- binary_subtraction bin1 bin2 []
  putStrLn "     ---------------"
  numeric_result <- binary_to_numeric
   binary_result 0
  putStrLn ("     " ++ show binary_result)
  putStrLn ""
  return numeric_result





----------------------------
-- Logical Operations
----------------------------

logical_operations :: String -> Int -> Int -> IO Int

logical_operations "AND" num1 num2 = do
  -- The ADD operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  bin2 <- numeric_to_binary num2 [] -- Convert to binary
  let hexString1 = showHex num1 ""
  let hexString2 = showHex num2 ""
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ show hexString1)
  putStrLn (" AND " ++ show bin2 ++ " = " ++ show hexString2)
  binary_result <- binary_and bin1 bin2 []
  let binary_show = reverse binary_result -- We need to show the result not reversed
  putStrLn "     ---------------"
  putStrLn ("     " ++ show binary_show)
  putStrLn ""
  numeric_result <- binary_to_numeric binary_show 0
  return numeric_result

logical_operations "OR" num1 num2 = do
  -- The OR operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  bin2 <- numeric_to_binary num2 [] -- Convert to binary
  let hexString1 = showHex num1 ""
  let hexString2 = showHex num2 ""
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ show hexString1)
  putStrLn (" OR  " ++ show bin2 ++ " = " ++ show hexString2)
  binary_result <- binary_or bin1 bin2 []
  let binary_show = reverse binary_result -- We need to show the result not reversed
  putStrLn "     ---------------"
  putStrLn ("     " ++ show binary_show)
  putStrLn ""
  numeric_result <- binary_to_numeric binary_show 0
  return numeric_result

logical_operations "XOR" num1 num2 = do
  -- The XOR operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  bin2 <- numeric_to_binary num2 [] -- Convert to binary
  let hexString1 = showHex num1 ""
  let hexString2 = showHex num2 ""
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ show hexString1)
  putStrLn (" XOR " ++ show bin2 ++ " = " ++ show hexString2)
  binary_result <- binary_xor bin1 bin2 []
  let binary_show = reverse binary_result -- We need to show the result not reversed
  putStrLn "     ---------------"
  putStrLn ("     " ++ show binary_show)
  putStrLn ""
  numeric_result <- binary_to_numeric binary_show 0
  return numeric_result

logical_operations_not :: String -> Int -> IO Int
logical_operations_not "NOT" num1 = do
  -- The NOT operation
  bin1 <- numeric_to_binary num1 [] -- Convert to binary
  let hexString = showHex num1 ""
  putStrLn ""
  putStrLn ("     " ++ show bin1 ++ " = " ++ hexString)
  binary_result <- binary_not bin1 []
  let binary_show = reverse binary_result -- We need to show the result not reversed
  putStrLn "     ---------------"
  putStrLn ("     " ++ show binary_show)
  putStrLn ""
  numeric_result <- binary_to_numeric binary_show 0
  return numeric_result






----------------------------
-- MAIN LOOP
----------------------------

main :: IO ()
main = do
  putStr "What would you like to do today? Choose from the list (NOT, OR, AND, XOR, ADD, SUB, QUIT): "
  operation <- getLine
  if operation == "QUIT"
    then putStrLn "Goodbye!" 
    else do
      handleOperation operation
      main

-- Operation to handle user input
handleOperation :: String -> IO ()
handleOperation "NOT" = do
  putStr "What's your number? "
  number1 <- getLine
  let n1 = read number1 :: Int
  result <- logical_operations_not "NOT" n1
  let res = showHex result ""
  putStrLn $ "     Your Result is: " ++ res
  putStrLn ""

handleOperation "AND" = do
  putStr "What's your first number? "
  number1 <- getLine
  putStr "What's your second number? "
  number2 <- getLine
  let n1 = read number1 :: Int
      n2 = read number2 :: Int
  result <- logical_operations "AND" n1 n2
  let res = showHex result ""
  putStrLn $ "     Your Result is: " ++ res 
  putStrLn ""

handleOperation "OR" = do
  putStr "What's your first number? "
  number1 <- getLine
  putStr "What's your second number? "
  number2 <- getLine
  let n1 = read number1 :: Int
      n2 = read number2 :: Int
  result <- logical_operations "OR" n1 n2
  let res = showHex result ""
  putStrLn $ "     Your Result is: " ++ res 
  putStrLn ""

handleOperation "XOR" = do
  putStr "What's your first number? "
  number1 <- getLine
  putStr "What's your second number? "
  number2 <- getLine
  let n1 = read number1 :: Int
      n2 = read number2 :: Int
  result <- logical_operations "XOR" n1 n2
  let res = showHex result ""
  putStrLn $ "     Your Result is: " ++ res 
  putStrLn ""

------------
-- Instead of signed vs unsigned binary, we handle arithmetic with the "adjustedResult" variable within an if-statement
------------

handleOperation "ADD" = do
  putStr "What's your first number? "
  number1 <- getLine
  putStr "What's your second number? "
  number2 <- getLine
  let n1 = read number1 :: Int
      n2 = read number2 :: Int
  result <- artithmetic_operations "ADD" n1 n2
  let adjustedResult = if n1 + n2 >= 0
                         then result
                         else result - 256
  putStrLn $ "     Your Result is: " ++ show adjustedResult
  putStrLn ""

handleOperation "SUB" = do
  putStr "What's your first number? "
  number1 <- getLine
  putStr "What's your second number? "
  number2 <- getLine
  let n1 = read number1 :: Int
      n2 = read number2 :: Int
  result <- artithmetic_operations "SUB" n1 n2
  let adjustedResult = if n1 - n2 >= 0
                         then result
                         else result - 256
  putStrLn $ "     Your Result is: " ++ show adjustedResult
  putStrLn ""

-- Finally, if there is an invald choice...
handleOperation operation = putStrLn "Invalid Choice... Try again."