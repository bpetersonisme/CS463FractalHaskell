module Main where
import Fractals
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
       ("mandelbrot":maxIters:rows:cols:lowR:lowI:hiR:hiI:      filename:[]) ->  toFile (M (read maxIters::Int) (read rows::Int) (read cols::Int) (C (read lowR) (read lowI)) (C (read hiR) (read hiI))                        ) filename
       ("julia"     :maxIters:rows:cols:lowR:lowI:hiR:hiI:cR:cI:filename:[]) ->  toFile (J (read maxIters::Int) (read rows::Int) (read cols::Int) (C (read lowR) (read lowI)) (C (read hiR) (read hiI)) (C (read cR) (read cI))) filename
       _ -> error ("need correct arguments!")
  putStrLn "done."



