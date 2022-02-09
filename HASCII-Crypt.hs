import Data.Char
import System.IO
import System.Environment

--The fine craft of λrc

--TODO:
--Clean up the IO code, and make it less redundant
--Figure out a more concise way to write "if, else if, else" conditions
--Google Drive's ' and " are messing stuff up, filter them out.
--Otherwise "Bullet Proof" the program. Add more friendly error messages.
--After that, upgrade to 1.1.5

--Version Number
version = "1.1.4"

--Define the type and value of full Ascii table
fullAscii :: [Char]
fullAscii = ['\NUL'..'\DEL']

--Define the type and value of my Ascii table
ascii :: [Char]
ascii = [' '..'~']

--Convert Ascii table into Integers representing their values
intAscii :: [Int]
intAscii = [0..(length ascii)-1]

--Preforms a Caesar Shift
cShift :: Int -> [a] -> [a]
cShift n xs = (snd (splitAt n xs) ++ fst (splitAt n xs))

--Creates a list of tuples that correspond are used in encryption
cShiftEncode :: Int -> [a] -> [(a,a)]
cShiftEncode n xs = zip xs (cShift n xs)

--Same thing, but for decryption
cShiftDecode :: Int -> [a] -> [(a,a)]
cShiftDecode n xs = zip (cShift n xs) xs

--Takes a string and turns it into a list of Ascii integers
toAscii :: String -> [Int]
toAscii n = [ord x - 32 | x <- n]

--"Undoes" toAscii
fromAscii :: [Int] -> String
fromAscii n = [if ((x + 32) < 128) then fullAscii !! (x + 32)
			  else error "Can not encrypt, sample contains non-Ascii characters"| x <- n]

--Replaces values in a list in accordance to the cShift functions
encrypt :: [Int] -> [(Int,Int)] -> [Int]
encrypt [] _ = []
encrypt (x:xs) shiftTable = do
                        let eLst = [n | n <- shiftTable, fst n == x]
                        if (null eLst)
                            then [x] ++ encrypt xs shiftTable
                            else [snd $ head eLst] ++ encrypt xs shiftTable

--IO Loop for encryption and decryption interactively
ioLoop = do
    putStr "\nWould you like to encrypt or decrypt? (e/d): "
    hFlush stdout
    ui <- getLine
    if (toLower (head ui) ==  'e')
        then do
            putStr "\nEnter a message to encrypt: "
            hFlush stdout
            message <- getLine
            putStr $ "\nEnter a key to encrypt by (1-" ++ (show $ length ascii - 2) ++ "): "
            hFlush stdout
            key <- getLine
            let eMessage = fromAscii (encrypt (toAscii message) (cShiftEncode (read key) intAscii))
            putStrLn $ "\nEncrypted Message: " ++ "\"" ++ eMessage ++ "\""
        else do
            putStr "\nEnter a message to decrypt: "
            hFlush stdout
            message <- getLine
            putStr $ "\nEnter a key to decrypt by (1-" ++ (show $ length ascii - 2) ++ "): "
            hFlush stdout
            key <- getLine
            let eMessage = fromAscii (encrypt (toAscii message) (cShiftDecode (read key) intAscii))
            putStrLn $ "\nDecrypted Message: " ++ "\"" ++ eMessage ++ "\""
    putStr "\nAgain? (y/n)"
    hFlush stdout
    ui <- getLine
    if (toLower (head ui) ==  'y')
        then ioLoop
        else return ()

--Preform whole file encryption or decryption
fileIO file = do
             message <- readFile file
             putStr "\nWould you like to encrypt or decrypt? (e/d): "
             hFlush stdout
             ui <- getLine
             if (toLower (head ui) ==  'e')
                 then do
                     putStr $ "\nEnter a key to encrypt by (1-" ++ (show $ length ascii - 2) ++ "): "
                     hFlush stdout
                     key <- getLine
                     let encryptMessage x = fromAscii (encrypt (toAscii x) (cShiftEncode (read key) intAscii))
                     putStr "\nWorking..."
                     hFlush stdout
                     writeFile ("E_" ++ file) ((unlines . map encryptMessage . lines) message)
                     putStrLn "Done."
                     putStrLn $ "\nFile saved as: " ++ ("E_" ++ file)
                 else do
                     putStr $ "\nEnter a key to decrypt by (1-" ++ (show $ length ascii - 2) ++ "): "
                     hFlush stdout
                     key <- getLine
                     let decryptMessage x = fromAscii (encrypt (toAscii x) (cShiftDecode (read key) intAscii))
                     putStr "\nWorking..."
                     hFlush stdout
                     writeFile ("D_" ++ file) ((unlines . map decryptMessage . lines) message)
                     putStrLn "Done."
                     putStrLn $ "\nFile saved as: " ++ ("D_" ++ file)

--Handle IO... Duh
main :: IO()
main = do
    fileIn <- getArgs
    if (null fileIn)
        then do putStrLn $ "Welcome to HASCII Crypt V" ++ version
                ioLoop
        else do
                if (length fileIn > 1)
                    then error "Too many arguments, only one filepath is accepted."
                    else fileIO $ head fileIn