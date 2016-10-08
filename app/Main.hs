import Lib
import qualified Ivory.Compile.C.CmdlineFrontend as C (compile)

main :: IO ()
main = C.compile [cadet] []
