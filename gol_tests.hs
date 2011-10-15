
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
reversedBirth :: CrdLst -> Gen CrdLst
reversedBirth = undefined

