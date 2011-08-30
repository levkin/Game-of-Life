
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import GoL.Definitions 
import GoL.Backend.IF
import GoL.Generation
import Data.List
--import Data.IntMap
import Control.Monad
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.Gen
import GoL.Game
import System.Random
import GoL.Backend.IF
import GoL.Backend.Txt
import qualified Data.ByteString.Char8 as B

----------------------------------------------------------------
toCellSt :: [Coord] -> CellStorage
toCellSt lst = GoL.Definitions.fromList $ [ Cell {coord = x,neighborCnt = 0,lifeCnt = 0} | x <- lst ]

----------------------------------------------------------------
fromCellSt :: CellStorage -> [Coord]
fromCellSt st =  map coord  $ GoL.Definitions.toList st

fromCellChg :: [CellChange] -> [Coord]
fromCellChg  = map (GoL.Definitions.coord . snd) 

toCrd z = Crd {crd = z} -- Shorter notation for crd creation

toCoordHelper :: CrdLst -> [Coord]
toCoordHelper x = map crd $ crdLst x
--------------------------------------------------------------------------------

-- Implement random coordinate
newtype Crd = Crd { crd :: Coord } deriving (Eq,Show)  -- Coordinate

instance Arbitrary Crd where 
  arbitrary = elements [ Crd { crd = (x,y)} | x <- [1.._maxX],y <- [1.._maxY] ]


-- Implement random list length
newtype CrdLstLength = CrdLstLength { crdLstLength :: Int }

instance Arbitrary CrdLstLength where
  arbitrary = elements [ CrdLstLength { crdLstLength = x } | x <- [1..((_maxY * _maxX) `div` 4)]]
  --arbitrary = elements [ CrdLstLength { crdLstLength = x } | x <- [1..6]]

-- Implement random cell lists
newtype CrdLst = CrdLst { crdLst :: [Crd] } deriving (Eq,Show)

instance Arbitrary CrdLst where
  arbitrary = do
    lstLength <- (arbitrary :: Gen CrdLstLength) -- Randomize  length
    anyList <- arbitrary :: Gen [Crd]
    let currLst = CrdLst { crdLst =  take ( crdLstLength lstLength ) $ nub anyList } 
    return currLst 
    

--------------------------------------------------
-- Implement expected birth results
--------------------------------------------------
newtype PreBirthPair = Pre { runPrebirth :: (CrdLst,CrdLst) } deriving (Eq,Show) -- PreBirth + expBirth result

instance Arbitrary PreBirthPair where
  arbitrary = do
    exp_birth <- (arbitrary :: Gen CrdLst)

    exp_pre_birth <- reversedBirth exp_birth

    let (exp_pre_birth',exp_birth') = (crdLst exp_pre_birth,crdLst exp_birth)
        exp_birth_uniq = exp_birth' \\ exp_pre_birth'

    return $ Pre (exp_pre_birth,CrdLst $ exp_birth_uniq)

    
-- Make two crdLsts independent , such that x ++ y = union (x,y)
-- Assuming both list don't contain repetitions

mkIndLstHelper :: (CrdLst,CrdLst) -> (CrdLst,CrdLst)
mkIndLstHelper (lst1,lst2) = (res_lst1,res_lst2) where
  (lst1',lst2') = (crdLst lst1,crdLst lst2)
  (pre_res_lst1,pre_res_lst2) = (lst1' \\ lst2',lst2' \\ lst1')
  (res_lst1,res_lst2) = (CrdLst $  pre_res_lst1,CrdLst $ pre_res_lst2)


-- Implement function performing reversed birth
-- Since all generation is in scope of Gen monad . It will not return pure birth
-- but rather lifted one
-- Traverse list of expected results and generate minimum number of neighbours ,required for generation
-- of new cells



-- Get requested number of random neighbours
-- Not too clean , since max value neighbours can be 8 and int is not proper type
-- Implement sort of fold
genSomeNeighbours :: Int -> Crd -> Gen [Crd]
genSomeNeighbours 0 _ = return []
genSomeNeighbours num c = gen' num [] where

  (x_,y_) = crd c 

  gen' :: Int -> [Crd] -> Gen [Crd]
  gen' 0  acc = return acc -- Return accumulator
  gen' x  acc = do 
    let allComb = [ toCrd (x,y) | x <- [x_-1..x_+1], x > 0, y <- [y_-1..y_+1], y > 0 ,x <= _maxX, y <= _maxY , (x,y) /= (x_,y_) , (toCrd (x,y)) `notElem` acc ]
    if (null allComb) then return acc
    else do
      res <- elements allComb
      gen' (x-1) (acc ++ [res])
      
--------------------------------------------------

-- Generate parents to
reversedBirth :: CrdLst -> Gen CrdLst
reversedBirth lst = result where
  lstSize = length $ crdLst lst -- size of list of neighbours
  app f x = f x
  app2 f x y = f x y
  replGenSomeNLst = replicate lstSize genSomeNeighbours -- Build list of partial functions
  result = do
    let numNbr = elements [_birthNeigbourCnt..(_birthNeigbourCnt+2)] 

    neighbrNumLst <- sequence $ replicate lstSize numNbr

    lstCrdlst  <- sequence $ zipWith3 (app2)  replGenSomeNLst neighbrNumLst (crdLst lst)

    let concLstCrdLst = nub $ concat lstCrdlst -- Unique inputs

    return $ CrdLst { crdLst = concLstCrdLst }           
     
--------------------------------------------------

--------------------------------------------------
-- Properties
--------------------------------------------------
-- 1) Result of applying 'birth' to prebirth contains birth result . Intersect of two lists is expected list
propBirthContains :: PreBirthPair -> Bool

propBirthContains p@(Pre (pre_b,exp_b)) = exp_b_lst == (intersect exp_b_lst birth_res) where
   
  exp_b_lst = toCoord_ exp_b
  birth_res = fromCellChg $ GoL.Generation.births $ toCellSt $ toCoord_ pre_b
  toCoord_ x = map crd $ crdLst x


--------------------------------------------------
-- Play game
--------------------------------------------------

-- Build turns datastructure
turns :: Turn
turns = turns_ 10 where
  turns_ n | n == 0 = Last
           | otherwise = Continue $ turns_ (n-1)


-- generate basic position
-- It is based of birth property generations
-- Seed -> List of coordinates
gen_basic_position :: Int -> [Coord]
gen_basic_position seed = res_lst where 
  res_lst = toCoordHelper crd_lst
  crd_lst :: CrdLst
  gen_crd_lst :: Gen CrdLst
  gen_crd_lst =  do
    x <- arbitrary
    y <- reversedBirth x
    return y
  crd_lst = unGen gen_crd_lst (mkStdGen 10) seed    

-- Initial storage -- Check with seed 3
initCellSt = toCellSt $ gen_basic_position 3

initTestBe = GolTextBe {
    generation = 0,
    array = B.empty,
    dim = (_maxX,_maxY),
    fileName = "./gol_output_test.txt"
    -- Currently don't keep handle . Handle will be AD HOC , where file will be appended with each update
  } 

-- Play 10 turns of game of life
playGame = do
  be <- GoL.Backend.IF.init initTestBe (_maxX,_maxY) 
  let initGameSt = GameState initCellSt be 
  game turns initGameSt   

--------------------------------------------------
-- End play game
--------------------------------------------------




-----------------------------------------------------
--  Debug
-----------------------------------------------------






----------------------------------------------------
--  End debug
----------------------------------------------------

