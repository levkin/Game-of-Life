
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GoL.Definitions 
import GoL.Backend.IF
import GoL.Generation
import Data.List
--import Data.IntMap
import Control.Monad
import Control.Monad.State
import Test.QuickCheck

----------------------------------------------------------------
toCellSt :: [Coord] -> CellStorage
toCellSt lst = GoL.Definitions.fromList $ [ Cell {coord = x,neighborCnt = 0,lifeCnt = 0} | x <- lst ]

----------------------------------------------------------------
fromCellSt :: CellStorage -> [Coord]
fromCellSt st =  map coord  $ GoL.Definitions.toList st


--------------------------------------------------------------------------------

-- Implement random coordinate
newtype Crd = Crd { crd :: Coord } deriving (Eq,Show)  -- Coordinate

instance Arbitrary Crd where 
  arbitrary = elements [ Crd { crd = (x,y)} | x <- [0.._maxX],y <- [0.._maxY] ]

-- Implement random list length
newtype CrdLstLength = CrdLstLength { crdLstLength :: Int }

instance Arbitrary CrdLstLength where
  arbitrary = elements [ CrdLstLength { crdLstLength = x } | x <- [0..((_maxY * _maxX) `div` 4)]]

-- Implement random cell lists
newtype CrdLst = CrdLst { crdLst :: [Crd] }

instance Arbitrary CrdLst where
  arbitrary = do
    lstLength <- (arbitrary :: Gen CrdLstLength) -- Randomize  length
    anyList <- arbitrary :: Gen [Crd]
    let currLst = CrdLst { crdLst =  take ( crdLstLength lstLength ) $ nub anyList } 
    return currLst 
    
-- Implement function performing reversed birth
-- Since all generation is in scope of Gen monad . It will not return pure birth
-- but rather lifted one
-- Traverse list of expected results and generate minimum number of neighbours ,required for generation
-- of new cells



-- Get requested number of random neighbours
-- Not too clean , since max value neighbours can be 8 and int is not proper type
genSomeNeighbours :: Int -> Crd -> Gen [Crd]
genSomeNeighbours num c = result where 
  (x_,y_) = crd c
  anyN = elements [ Crd { crd = (x,y)} |  x <- [x_-1..x_+1], x > 0, y <- [y_-1..y_+1], y > 0 ,x <= _maxX, y <= _maxY , (x,y) /= (x_,y_)]
  result =  do 
      lst <- sequence $ replicate num anyN 
      return nub lst -- Make list unique . 

reversedBirth :: CrdLst -> Gen CrdLst
reversedBirth = undefined
  
     

