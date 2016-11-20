module Levenshtein (ldist) where

import System.Environment

main = do
    a <- getArgs
    let a1 = a !! 0
        a2 = a !! 1
    print $ ldist a1 a2


ldist :: (Eq a) => [a] -> [a] -> Int
ldist a1 a2
    | min i j == 0 = max i j
    | otherwise = minimum [ldist ia1 a2 + 1, ldist a1 ia2 + 1, ldist ia1 ia2 + if last a1 == last a2 then 0 else 1]
    where
        i = length a1
        j = length a2
        ia1 = init a1
        ia2 = init a2
