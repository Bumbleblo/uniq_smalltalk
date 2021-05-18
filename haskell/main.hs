import System.Environment
import System.IO

-- split arguments
split :: Char -> String -> [String]
split delim [] = [""]
split delim (c:cs)
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split delim cs



-- -- skip N chars from a string 
skip ::  Integer -> String -> String
skip n "" = ""
skip 0 string = string
skip n string = skip (n-1) (tail string)

-- compare lines
compareLines :: String -> String -> Bool
compareLines "" "" = True
compareLines "" line2 = False
compareLines line1 "" = False
compareLines (x:xs) (y:ys) = if x == y 
                            then compareLines xs ys
                            else False

-- uniqAcc
uniqAcc line [] skipN =  [line]
uniqAcc actualLine (x:xs) skipN = if compareLines (skip skipN actualLine) (skip skipN  x)
                            then (uniqAcc actualLine xs skipN)
                            else [actualLine] ++ (uniqAcc x  xs skipN )

-- uniq
uniq [] skipN = []
uniq (x:xs) skipN = uniqAcc x xs skipN

main::IO()
main = do

    args <- getArgs

    let filename = head args

    inh <- openFile filename ReadMode
    input <- hGetContents inh

    let uniqueLines = uniq (lines input) 0
    mapM_ putStrLn uniqueLines

