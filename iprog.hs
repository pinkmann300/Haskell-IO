import Distribution.Simple.Utils (xargs)
-- Interactive programming in Haskell based on Programming in Haskell by Graham Hutton 

-- type IO a = World -> (a, World) where World refers to the state of the system and we usually return a tuple because there are two things
-- under consideration when we take an interactive program. The value that we want the interactive program to evaluate down to and 
-- the side effects which come along as part and parcel of the interactive program itself (Eg - state changes, mutations and allocations). 

-- :t putChar is Char -> IO () because you provide it with an argument to proces 
-- :t getChar is IO Char because it reads a character (THIS IS AN INTERACTIVE ACTION) from the keyboard and returns the same character.

-- :t return is very similar to the return we have seen otherwise in other functional programming languages and in a non-interactive batch 
-- program style in Haskell as well. It just returns the value but since it is of an interactive nature, it has the type of whatever value 
-- it returns embedded into the IO return tuple. 

-- The function below reads three characters, discards the second one and returns the first one and third one seperately in a tuple.

act :: IO (Char, Char)
act = do 
    x <- getChar
    _ <- getChar
    z <- getChar
    putChar '\n'
    return (x,z)

-- We will now rewrite existing functions getLine as getLine1 and putStr and so on using the existing primitives.

getLine1 :: IO String
getLine1 = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do
            xs <- getLine1
            return (x:xs)

-- Now we putStr1 

putStr1 :: String -> IO()
putStr1 [] = return ()
putStr1 (x:xs) = do
    putChar x
    putStr1 xs 


putStrLn1 :: String -> IO()
putStrLn1 xs = do 
    putStr1 xs
    putChar '\n'


strLen :: IO()
strLen = do
    putStrLn1 "Enter a string: "
    ks <- getLine1
    putStr1 "The string has " 
    putStr1 (show(length ks))
    putStr1 " characters"
    putChar '\n'



    