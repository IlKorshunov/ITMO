module Main (main) where

import System.Console.Haskeline
import HW5.Base (HiValue)
import HW5.Parser (parse)
import HW5.Evaluator (eval)
import HW5.Pretty (prettyValue)
import Prettyprinter.Render.Terminal (putDoc)
import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = runInputT defaultSettings loop 

loop :: InputT IO ()
loop = do
    input <- getInputLine "hi> " 
    case input of
        Nothing -> return ()
        Just expr -> do
            case parse expr of
                Left e -> outputStrLn $ "Parse error: " ++ show e
                Right tree -> do
                    result <- liftIO $ eval tree
                    case result of
                        Left err -> outputStrLn $ "Evaluation error: " ++ show err
                        Right value -> liftIO $ printValue value
            loop


printValue :: HiValue -> IO ()
printValue value = do
    let doc = prettyValue value
    putDoc doc               
    putStrLn ""