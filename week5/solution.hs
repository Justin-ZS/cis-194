import Data.Char
import Data.List
import Data.Ord

halveEvens :: [Integer] -> [Integer]
halveEvens [] = []
halveEvens (x:xs)
  | even x = (x `div` 2) : halveEvens xs
  | otherwise = halveEvens xs
-- halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString [] = []
safeString (x:xs)
  | isControl x = '_' : safeString xs
  | isAscii x = x : safeString xs
  | otherwise = '_' : safeString xs
{-
safeString = map fix
  where fix c | isControl c = '_'
              | isAscii c   = c
              | otherwise   = '_'
-}

holes :: [a] -> [[a]]
holes [] = []
holes xs = getHoled 0 []
  where getHoled n ys | (length (hole n) == length xs - 1) = getHoled (n + 1) (ys ++ [hole n])
                      | otherwise = ys
          where hole n = case splitAt n xs of
                        (fs, []) -> fs
                        (fs, ss) -> fs ++ tail ss
-- holes xs = zipWith (++) (inits xs) (map tail (init (tails xs)))
  
longestText :: Show a => [a] -> a
longestText xs = foldl (\acc cur -> if cur `isLongerText` acc then cur else acc) (head xs) xs
  where isLongerText a b = length (show a) >= length (show b)
-- longestText = maximumBy (comparing (length . show))
  
adjacents :: [a] -> [(a,a)]
adjacents [] = []
adjacents [a] = []
adjacents (x:y:xs) = (x, y) : adjacents (y:xs)
-- adjacents xs = zip xs (tail xs)

commas :: [String] -> String
commas [] = ""
commas [s] = s
commas (s:ss) = s ++ ", " ++ commas ss
-- commas = intercalate ", "

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials xs = foldl (zipWith (+)) (replicate (length (head xs)) 0) xs
-- addPolynomials = foldl1 (zipWith (+))
-- A variant of foldl that has no base case, and thus may only be applied to non-empty structures.

sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter (isDigit . head) . groupBy (\x y -> isDigit x && isDigit y)
-- sumNumbers = sum . map read . filter (isDigit.head) . groupBy ((==) `on` isDigit)
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- (*) `on` f = \x y -> f x * f y.

wordCount :: String -> String
wordCount input = unlines [
  "Number of lines: " ++ show (length ls),
  "Number of empty lines: " ++ show (length (filter null ls)),
  "Number of words: " ++ show (length ws),
  "Number of unique words: " ++ show (length (nub ws)),
  "Number of words followed by themselves: " ++ show (length (filter (uncurry (==)) (adjacents ws))),
  "Length of the longest line: " ++ show (length (longestText ls))
  ]
  where
    ls = lines input
    ws = words input