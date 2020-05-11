import Control.Monad

doThing :: String -> IO ()
doThing line = putStrLn line

caseParser :: Parser Case
caseParser = do
    -- EXAMPLE: parses a case consisting of 3 lines: the first describes the
    -- number n of elements in the following two lines, the next two lines
    -- have n space-separated elements:
    w <- word
    let n = read w
    as <- count n word
    bs <- count n word
    return (map read as, map read bs)

mainParser :: Parser Input
mainParser = do
    n <- word
    ms <- count (read n) caseParser
    return ms

main :: IO ()
main = do
    putStrLn "Lol"
    contents <- getContents
    let inp = parseWith mainParser contents
    forM_ (lines contents) doThing

-- http://brandon.si/code/haskell-boilerplate-for-google-codejam/


word = do
    w <- many1 nonWhite
    spaces
    return w
