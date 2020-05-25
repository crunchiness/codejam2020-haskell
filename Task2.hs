import Text.Printf

main = readLn >>= \n -> mapM solve [1..n]

f :: String -> Int -> String -> String
f []     level output = output ++ [')' | _ <- [1..level]]
f (x:xs) level output = f xs y $ output ++ (updateOutput x y level)
    where
        y = read [x] :: Int
        updateOutput x y level | y == level = [x]
                               | y < level  = [')' | _ <- [1..level - y]] ++ [x]
                               | otherwise  = ['(' | _ <- [1..y - level]] ++ [x]

doNestingDepth :: String -> String
doNestingDepth s = f s 0 ""

solve :: Int -> IO ()
solve i = do
    s <- getLine
    putStrLn $ printf "Case #%d: %s" i (doNestingDepth s)
