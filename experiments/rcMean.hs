module RCMean where

import Data.List (transpose)

type ParSite = Int


{- When we have a data file that includes multiple columns, but we only care
 - about one column we can convert it to a PSiteInfo using getInfo.
 - We just provide which column we'd like to associate with the PSiteInfo as
 - the first argument to getInfo. Passing 0 would be silly.
 -}
data PSiteInfo = P ParSite [Int]
    deriving Show

getInfo :: Int -> [String] -> PSiteInfo
getInfo c xs = P (myRead (head xs)) [myRead (xs !! c)]
  where
    myRead str = (read str) :: Int

getAllInfo :: Int -> String -> [PSiteInfo]
getAllInfo c = map (getInfo c . words) . drop 1 . lines

{- For things like gnuplot's boxplot, we have to provide the data in a stupid format.
 - Each parsite gets it's own line, followed by all of the 'y' values.
 - combineAdj can get us most of the way there by crunching down adjacent PSiteInfos
 - that are for the same parsite. This works because we know the data comes sorted
 - by parsite
 -}
combineAdj :: [PSiteInfo] -> [PSiteInfo]
combineAdj []  = []
combineAdj [x] = [x]
combineAdj (P ps i : P qs j : xs)
    | ps == qs  = combineAdj (P ps (i ++ j) : xs)
    | otherwise = P ps i : combineAdj (P qs j : xs)

unwordsTab :: [String] -> String
unwordsTab [] = ""
unwordsTab xs = foldr1 (\w s -> w ++ '\t':s) xs

formatPSite :: PSiteInfo -> String
formatPSite (P _ is) = unwordsTab $ map show is

formatBoxPlot :: [PSiteInfo] -> String
formatBoxPlot = unlines . map unwordsTab . transpose . drop 1 . makeSameLength . map (words . formatPSite)



--exercise for myself
makeSameLength :: [[String]] -> [[String]]
makeSameLength xs = map (take maxLen) infLists
  where
    maxLen   = maximum $ map length xs
    infLists = map (++ repeat "") xs
