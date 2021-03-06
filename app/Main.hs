module Main where

import System.Console.Haskeline
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import Control.Monad.IO.Class (liftIO)
import HW3.Action (runHIO, HiPermission (..))
import Data.Set (fromList)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do let res = parse input
                                case res of
                                    Right x -> do
                                        v <- liftIO $ runHIO (eval x) (fromList [AllowRead, AllowWrite, AllowTime])
                                        case v of
                                            Right y -> outputStrLn $ show (prettyValue y)
                                            Left y -> outputStrLn $ show y
                                    Left x -> outputStrLn $ errorBundlePretty x
                                loop
