import Language
import Heap
import Parser
import Compiler
import System.Environment
import System.FilePath.Posix

main = do
    (file:xs) <- getArgs
    source <- readFile file
    let outputFile = replaceExtension file "gcode"
    writeGCodeFile outputFile source


