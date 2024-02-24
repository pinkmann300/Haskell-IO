-- Define an action adder :: IO () that reads a given number of integers from the keyboard, one
-- per line, and displays their sum

readAndSum :: Int -> Int -> IO Int
readAndSum t 0 = return t 
readAndSum t r = do
  putStrLn "Enter the next integer:"
  num <- getLine -- Read the next integer
  let num1 = read num :: Int
  readAndSum (t + num1) (r - 1)

adder :: IO ()
adder = do
    putStr "How many numbers ?"
    putChar '\n'
    xs <- getLine
    let sum1 = 0
    let ks = read xs :: Int
    if ks == 0 then
        putStr (show sum1)
    else
        do
            ms <- readAndSum sum1 ks
            putStr "The total is: "
            putStr (show ms)



