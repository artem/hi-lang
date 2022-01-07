module Main where

import System.Console.Haskeline
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
            --    Just input -> do let res = parse input >>= eval
            --                     case res of
            --                         Right (Right x) -> outputStrLn $ show (prettyValue x)
            --                         _ -> outputStrLn "eggog"
            --                     loop
               Just input -> do let res = parse input
                                case res of
                                    Right x -> outputStrLn $ show x
                                    _ -> outputStrLn "eggog"
                                loop
