module CLI where

import Cards
import Poker.Eval
import System.Environment

realMain :: IO ()
realMain = do
  args <- getArgs

  let cards = readCards args
      o = outs cards

  putStrLn $ "Outs: " ++ show (length o)
  print o
