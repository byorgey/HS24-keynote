{-# LANGUAGE ImportQualifiedPost #-}

import Cats
import Data.Map qualified as M
import Data.Text.IO qualified as T
import System.Environment (getArgs)

main :: IO ()
main = do
  [f] <- getArgs
  src <- T.readFile f
  case readTerm src of
    Left err -> T.putStr err
    Right tm ->
      case infer M.empty tm of
        Left err -> T.putStrLn (prettyTypeError err)
        Right _ -> T.putStr (showT (interp M.empty tm))
