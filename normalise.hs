import System.Environment

norm i [a, num] = [a, show num']
  where
    num' = i/ (read num)
main = do
    [iters, sequ] <- getArgs
    conts <- readFile iters
    seqC <- readFile sequ
    let ls = map words $ lines conts
    let a  = read seqC
    putStr $ unlines $ fmap (unwords . norm a) ls
