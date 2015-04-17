import Language
import Heap
import Parser
import Compiler
import Data.List (partition)
import Data.Either (partitionEithers)
import System.Environment
import System.Exit
import System.FilePath.Posix

options = [("tco", TCO)
          ,("O0", OZ)
          ]

parseArg :: String -> Either String Option
parseArg ('-':op) = maybe err Right lu
  where
    lu  = lookup op options
    err = Left $ "CL option " ++ op ++ " is not supported"
parseArg f = Right $ File f

dealWithBadCLAs :: [String] -> IO ()
dealWithBadCLAs [] = return ()
dealWithBadCLAs xs = putStr (unlines xs) >> exitFailure

splitFiles :: [Option] -> ([String], [Option])
splitFiles xs = ([s | File s <- ys], zs)
  where
    (ys, zs) = partition isFile xs

main = do

    -- Parse args and exit if a bad argument is given
    xs <- getArgs
    let (es, as) = partitionEithers $ fmap parseArg xs
        (fs, os) = splitFiles as
    dealWithBadCLAs es

    -- read source files
    sources <- sequence $ fmap readFile fs
    let outputFiles = fmap (flip replaceExtension "gcode") fs
    sequence $ zipWith (writeGCodeFile os) outputFiles sources


