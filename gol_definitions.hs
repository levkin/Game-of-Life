
{-  Game of Life will write text file for each iteration
 -  which will be displayed in XL
 -    
 -  Basically : GameOfLife -> Cells [n+1] = Cells[n] + Births[n] - Deaths[n] 
 
 -}


module GoL.Definitions where

  import Control.Monad
  import Data.Ord

  ----------------------------------------------------
  --  Data types
  ----------------------------------------------------
 
  type X_c = Int
  type Y_c = Int
  type Coord = (X_c,Y_c) 


  data CellState = Alive | Dead deriving (Eq,Show)

  data Cell = Cell {
    coord :: Coord, 
    neighborCnt :: Int,
    lifeCnt :: Int
  } deriving (Show,Eq)

  
  type CellChange = (CellState,Cell)
  
  type CellStorage = [Cell] -- This is temporaty definition

  toList :: CellStorage -> [Cell]
  toList = id

  fromList :: [Cell] -> CellStorage 
  fromList = id

  
  my_insert :: CellStorage -> Cell -> CellStorage
  my_insert xs x = x:xs

  remove :: CellStorage -> Cell -> CellStorage
  remove x y = filter (\z -> z /= y) x

  _birthNeigbourCnt :: Int
  _birthNeigbourCnt = 3  
  
  _maxLifeCnt :: Int
  _maxLifeCnt = 3

  _maxX :: Int 
  _maxY :: Int

  _maxY = 20
  _maxX = 20

  getEmptyStorage :: CellStorage
  getEmptyStorage = [] -- Temporary . List will be changed to intMap
  
  data BCandSt = BCandSt {
    candidates :: CellStorage
  }
   
  -- Lazy stream indication whether to continue 
  data Simulation = Done | Cont (Simulation)


  -- Simulation specific parameters
  data Conf = Conf {
    numForBirth :: Int,
    dCount :: Int -- Number of cycles cell is alive without neghbours
  } deriving (Eq,Show)


