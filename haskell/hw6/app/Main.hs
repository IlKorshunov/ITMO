module Main (main) where
import HW6.T3
import Data.MyParser  

main :: IO ()
main = do
  putStrLn "Введите параметры конфигурации:"
  input <- getLine
  
  case parse input of
    Left err -> putStrLn $ "Ошибка парсинга: " ++ show err
    Right config -> do
      print config 
      let grids = simulate config
      mapM_ (printGrid config) grids

-- "-p 0.9 -i 1 -ill 2 -imm 1 -gs 5 -it 10"
-- -p 0.6 -i 1 -ill 2 -imm 2 -gs 11 -it 7
-- -p 0.4 -i 2 -ill 5 -imm 7 -gs 11 -it 10 