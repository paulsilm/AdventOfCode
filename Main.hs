import System.Environment
import System.IO
import Ex4

process :: String -> String
process = show.solve

main = do
  args <- getArgs
  let reader = if args == [] then getContents else readFile (head args)
  text <- reader
  print (process text)
