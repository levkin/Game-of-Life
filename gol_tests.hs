
import GoL.Definitions 
import GoL.Backend.IF
import GoL.Generation
import Data.List
--import Data.IntMap
import Control.Monad
import Control.Monad.State

-- Birth state 0 : Create 
-- Empty cell with _birthNeigbourCnt
birth_scenario_0 :: CellStorage
birth_scenario_0 = newSt where
  c0 = Cell {
    coord = (5,5),
    lifeCnt = 0,
    neighborCnt = 0
  }    

  c1 = Cell {
    coord = (5,6),
    lifeCnt = 0,
    neighborCnt = 0
  }    

  c2 = Cell {
    coord = (6,5),
    lifeCnt = 0,
    neighborCnt = 0
  }
  initSt = getEmptyStorage
  newSt = foldl' my_insert getEmptyStorage [c0,c1,c2]

----------------------------------------------------------------
toCellSt :: [Coord] -> CellStorage
toCellSt lst = GoL.Definitions.fromList $ [ Cell {coord = x,neighborCnt = 0,lifeCnt = 0} | x <- lst ]

