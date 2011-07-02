{-#LANGUAGE RecordWildCards,NamedFieldPuns #-}

module Gol.Generation where
  import GoL.Definitions 
  import GoL.Backend.IF
  import Data.List
  --import Data.IntMap
  import Control.Monad
  import Control.Monad.State



  -- State update of GoL
  simulate_generation ::  CellStorage -> ([CellChange], CellStorage)
  simulate_generation x = (changes , new_state) where
    y = fromList $ map (\z -> Cell { lifeCnt = (lifeCnt z) - 1,coord = coord z,neighborCnt = neighborCnt z } ) $ toList x -- Decrement lifeCnt by 1 in all cells
    changes = (deaths y) ++ (births y) -- Mutations will be added between generations
    new_state = update y changes


  -- Get Cell state --> traverse through state and update counter
  deaths ::  CellStorage -> [CellChange]
  deaths storage = cellChange where
    valList = map (\x-> (x,storage)) $ toList storage
    deathCands = snd $ (runState $ mapM_ findDeathCandidates valList) $ getEmptyStorage
    cellChange = map (toCellChange Dead) $ toList storage
  

  births ::  CellStorage -> [CellChange]
  births storage = cellChange where
    valList = map (\x-> (x,storage)) $ toList storage
    birthCands = snd $ (runState $ mapM_ findBirthCandidates valList) $ getEmptyStorage
    cellChange =  map (toCellChange Alive) $ filter (\x -> (neighborCnt x) >= _birthNeigbourCnt) $ toList storage
           
  -- Update cellstate from list of changes 
  -- Either add new cells or remove existing
  update :: CellStorage -> [CellChange] ->  CellStorage
  update storage changes = newStorage where
    newStorage = foldl' processChange storage changes
    processChange st_ change@(ch,cell) | ch == Alive = my_insert st_ new_cell 
                                       | otherwise = remove st_ cell where
      new_cell = Cell  { lifeCnt = _maxLifeCnt , coord = coord cell , neighborCnt = neighborCnt cell }

  --
  findBirthCandidates :: (Cell,CellStorage) -> State (CellStorage) (Cell,CellStorage)
  findBirthCandidates (cell,cellSt)  = state $ resFunc where  
    resFunc :: CellStorage -> ((Cell,CellStorage),CellStorage)
    resFunc curr_birthCands = ((cell,cellSt),new_birthCands) where
      nb = filter (\z-> notElem z cellSt) $ getNeighbors cellSt cell -- Fetch neighbors , that are not alive
      new_birthCands = putCell curr_birthCands nb -- Put them to


  findDeathCandidates :: (Cell,CellStorage) -> State (CellStorage) (Cell,CellStorage)
  findDeathCandidates (cell,cellSt) = state $ resFunc where
    resFunc :: CellStorage -> ((Cell,CellStorage),CellStorage)
    resFunc currDeathCands | (lifeCnt cell) < 0 = ((cell,cellSt),my_insert currDeathCands cell)
                           | otherwise = ((cell,cellSt),currDeathCands)
       
      
  -- Should define API for storage of cells

  getNeighbors :: CellStorage -> Cell -> CellStorage
  getNeighbors cStorage c = new_cells where 
    x_c = fst $ coord c
    y_c = snd $ coord c
    allNeighbors = fromList $ filter (\x -> notElem x cStorage) $
                   [ Cell { coord = (x,y),lifeCnt = _maxLifeCnt,neighborCnt = 0 } | x <- [(x_c-1)..(x_c+1)],x >= 0,x <= _maxX,
                                                                    y <- [(y_c-1)..(y_c+1)],y >= 0,y <= _maxY  ]
    
    new_cells = foldl' my_insert cStorage allNeighbors 



  putCell :: CellStorage -> [Cell] -> CellStorage
  putCell = undefined



  -- Mutations -- Spontaneous appearances of cells
  mutations ::  CellStorage -> [CellChange]
  mutations _ = [] -- Todo : Add mutations



  toCellChange :: CellState -> Cell -> CellChange
  toCellChange cg cell = (cg,cell)

