import Cats
import Data.Text.IO qualified as T
import System.Environment (getArgs)
import Text.Megaparsec (eof, runParser)

main :: IO ()
main = do
  [fileName] <- getArgs
  txt <- T.readFile fileName
  case runParser (parseTerm <* eof) fileName txt of
    Left err -> putStrLn (show err)
    Right term -> print (interp term)
