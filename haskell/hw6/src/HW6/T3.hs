{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  , simulate
  , printGrid
  ) where

import System.Random (StdGen, random,  mkStdGen)
import Control.Comonad (Comonad(..))
import Data.Grid
import Control.Monad ( liftM2 )
import Control.Lens
import qualified Data.ListZipper as LZ

data Config = Config
  { probability       :: Double
  , incubationPeriod  :: Int
  , illnessDuration   :: Int
  , immunityDuration  :: Int
  , iterations :: Int
  , gridSide :: Int
  } deriving Show

data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

data Cell = Cell
  { _cellState :: CellState
  , _cellRand  :: StdGen
  }
makeLenses ''Cell

instance Show Cell where
  show :: Cell -> String
  show (Cell Healthy _)      = "_"
  show (Cell (Infected _) _) = "i"
  show (Cell (Ill _) _)      = "#"
  show (Cell (Immune _) _)   = "@"

type Comonad19Grid = Grid Cell

-- printGrid :: Config -> Comonad19Grid -> IO ()
-- printGrid config g = do
--   let half = gridSide config `div` 2
--       rows = LZ.toList half (gridSide config - half - 1) (unGrid g)
--       printRow row = putStrLn $ concatMap show (LZ.toList half (gridSide config - half - 1) row)
--   mapM_ printRow rows
--   putStrLn ""

printGrid :: Config -> Comonad19Grid -> IO ()
printGrid config g = 
  let size = gridSide config `div` 2
      gridStr = gridToString size size show g
  in putStrLn gridStr  

uniqueGen :: Int -> Int -> Int -> StdGen
uniqueGen half x y = mkStdGen ((y + half) * (half * 2) + x)

generateCellAt :: Config -> Int -> Int -> CellState -> Cell
generateCellAt config x y state = Cell state (uniqueGen (gridSide config `div` 2) x y)

generateRow :: Config -> Int -> Int -> LZ.ListZipper Cell
generateRow config y half =
  let cells = [ if (x, y) == (0, 0)
                then generateCellAt config x y (Infected (incubationPeriod config))
                else generateCellAt config x y Healthy
              | x <- [-half .. half] ]
      center = cells !! half
      left = take half cells
      right = drop (half + 1) cells
  in LZ.LZ (reverse left) center right 

-- genR = map mkStdGen $ iterate (+2) 2
-- rightBranch = zipWith (flip Cell) genR stateLR

-- genL = map mkStdGen $ iterate (+2) 1
-- stateLR = repeat Healthy
-- startLine = zipWith (flip Cell) genL stateLR
-- leftCells = take half startLine
-- currentCell = startLine !! half
-- rightCells = take (gridSide config - half - 1) (drop (half + 1) startLine)
-- rowZipper = LZ.LZ leftCells currentCell rightCells
createGrid :: Config -> Comonad19Grid
createGrid config =
  let half = gridSide config `div` 2
      rows = [ generateRow config y half | y <- [-half .. half] ]
      focusRow = rows !! half
      leftRows = take half rows
      rightRows = drop (half + 1) rows
      gridZipper = LZ.LZ (reverse leftRows) focusRow rightRows  
  in Grid gridZipper

simulate :: Config -> [Comonad19Grid]
simulate config = take (iterations config) $ iterate (nextStep config) (createGrid config)

nextStep :: Config -> Comonad19Grid -> Comonad19Grid
nextStep config = extend (updateCurCell config)

updateCurCell :: Config -> Comonad19Grid -> Cell
updateCurCell config grid =
  let curCell = extract grid
      (newGen, shouldInfect) = infectOrSkipCurCell curCell grid (probability config) 
      newState = case curCell ^. cellState of
        Healthy ->
          if shouldInfect
            then Infected (incubationPeriod config)
            else Healthy

        Infected num ->
          if num <= 1
            then Ill (illnessDuration config)
            else Infected (num - 1)

        Ill num ->
          if num <= 1
            then Immune (immunityDuration config)
            else Ill (num - 1)

        Immune num ->
          if num <= 1
            then Healthy
            else Immune (num - 1)
            
      newCurCell = Cell newState newGen
      -- newCurCell = curCell & cellState .~ newState
      --                     & cellRand .~ newGen 
  in newCurCell

getStateNeig :: Cell -> Bool 
getStateNeig cell = case cell ^. cellState of
  Infected _ -> True
  Ill _ -> True
  _ -> False 

infectOrSkipCurCell :: Cell -> Comonad19Grid -> Double -> (StdGen, Bool)
infectOrSkipCurCell curCell grid prob =
  let neighbors = getNeighborCells grid
      len = length $ filter getStateNeig neighbors
      gen = curCell ^. cellRand
      (newGen, flag) = infectHealthCell len prob gen
  in (newGen, flag)


infectHealthCell :: Int -> Double -> StdGen -> (StdGen, Bool)
infectHealthCell size prob gen
    | size <= 0 = (gen, False)  
    | otherwise = 
        let (pr, nGen) = random gen
        in if pr < prob
             then (nGen, True)
             else infectHealthCell (size - 1) prob nGen


moveNeigbours :: [Grid a -> Grid a]
moveNeigbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [toLeft, toRight]
        verticals   = [toUp, toDown]

getNeighborCells :: Comonad19Grid -> [Cell]
getNeighborCells grid = extract <$> (moveNeigbours <*> pure grid)