{-#LANGUAGE RecordWildCards,NamedFieldPuns #-}

module GoL.Generation where
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
    cellChange =  map (toCellChange Alive) $ filter (\x -> (neighborCnt x) >= _birthNeigbourCnt) $ toList birthCands
           
-- Debug_4444
  valList1 st = map (\x-> (x,st)) $ toList st
  birthCands1 st = snd $ (runState $ mapM_ findBirthCandidates $ valList1 st ) $ getEmptyStorage

-- End Debug_4444



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
      --nb = filter (\z->  z `notElemByCoord` cellSt) $ getNeighbors cellSt cell -- Fetch neighbors that are not alive
      nb =  getNeighbors cellSt cell -- Fetch neighbors that are not alive
      new_birthCands = putNeighbourCell curr_birthCands nb -- Put them to birth candidates

  -- Debug_4444
  resFunc1 :: CellStorage -> (Cell,CellStorage) -> ((Cell,CellStorage),CellStorage)
  resFunc1 curr_birthCands (cell,cellSt) = ((cell,cellSt),new_birthCands) where
    --nb = filter (\z->  z `notElemByCoord` cellSt) $ getNeighbors cellSt cell -- Fetch neighbors that are not alive
    nb =  getNeighbors cellSt cell -- Fetch neighbors that are not alive
    new_birthCands = putNeighbourCell curr_birthCands nb -- Put them to birth candidates
  -- End Debug_4444

  findDeathCandidates :: (Cell,CellStorage) -> State (CellStorage) (Cell,CellStorage)
  findDeathCandidates (cell,cellSt) = state $ resFunc where
    resFunc :: CellStorage -> ((Cell,CellStorage),CellStorage)
    resFunc currDeathCands | (lifeCnt cell) < 0 = ((cell,cellSt),my_insert currDeathCands cell)
                           | otherwise = ((cell,cellSt),currDeathCands)
       
      
  -- Should define API for storage of cells

  -- Fetch all neighbours for a cell that are not already alive
  getNeighbors :: CellStorage -> Cell -> CellStorage
  getNeighbors cStorage c = new_cells where 
    x_c = fst $ coord c
    y_c = snd $ coord c
    allNeighbors = fromList $ filter (\x -> x `notElemByCoord`  cStorage) $
                   [ Cell { coord = (x,y),lifeCnt = _maxLifeCnt,neighborCnt = 0 } | x <- [(x_c-1)..(x_c+1)],x >= 0,x <= _maxX, 
                                                                    y <- [(y_c-1)..(y_c+1)],y >= 0,y <= _maxY,not $ and [x == x_c, y == y_c]   ]
    
    new_cells = foldl' my_insert getEmptyStorage allNeighbors 

-- Debug_3333
  allNeighbours st x1 y1 = filter (\x -> x `notElemByCoord`  st ) $ [ Cell { coord = (x,y),lifeCnt = _maxLifeCnt,neighborCnt = 0 } | x <- [(x1-1)..(x1+1)],x >= 0,x <= _maxX, 
                                                                    y <- [(y1-1)..(y1+1)],y >= 0,y <= _maxY,not $ and [x == x1, y == y1]   ]


  allNeighbours2  x1 y1 =  [ Cell { coord = (x,y),lifeCnt = _maxLifeCnt,neighborCnt = 0 } | x <- [(x1-1)..(x1+1)],x >= 0,x <= _maxX, 
                                                                    y <- [(y1-1)..(y1+1)],y >= 0,y <= _maxY,not $ and [x == x1, y == y1]   ]
-- End Debug_3333
  -- If cell is already found in storage -> Increment it's neighbor count
  -- Else insert it 
  putNeighbourCell :: CellStorage -> [Cell] -> CellStorage
  putNeighbourCell st cells = foldl' insert_tag st cells where
    insert_tag :: CellStorage -> Cell -> CellStorage
    insert_tag st cell  | cell `elemByCoord` st = my_insert new_st new_cell
                        | otherwise = my_insert st cell_upd where
      cell_upd = Cell { coord = coord cell , lifeCnt = lifeCnt cell , neighborCnt = 1 }

      new_st = fromList $ filter ( \x -> (coord x) /= coord cell) $ toList st -- Remove all occurrences with current coordinates
      curr_cells = fromList $ filter ( \x -> (coord x) == coord cell) $ toList st -- Insert new single occurrence with updated neighbours count
      new_cell = Cell { coord = coord cell,lifeCnt = lifeCnt cell,neighborCnt = (neighborCnt cell) + (length curr_cells)}

-- Debug_4444
  insert_tag1 :: CellStorage -> Cell -> CellStorage
  insert_tag1 st cell  | cell `elemByCoord` st = my_insert new_st new_cell
                      | otherwise = my_insert st cell_upd where
    cell_upd = Cell { coord = coord cell , lifeCnt = lifeCnt cell , neighborCnt = 1 }

    new_st = fromList $ filter ( \x -> (coord x) /= coord cell) $ toList st -- Remove all occurrences with current coordinates
    curr_cells = fromList $ filter ( \x -> (coord x) == coord cell) $ toList st -- Insert new single occurrence with updated neighbours count
    new_cell = Cell { coord = coord cell,lifeCnt = lifeCnt cell,neighborCnt = (neighborCnt cell) + (length curr_cells)}
-- End Debug_4444


  -- Mutations -- Spontaneous appearances of cells
  mutations ::  CellStorage -> [CellChange]
  mutations _ = [] -- Todo : Add mutations



  toCellChange :: CellState -> Cell -> CellChange
  toCellChange cg cell = (cg,cell)

  elemByCoord :: Cell -> CellStorage -> Bool
  elemByCoord cell cellSt = elemBC_ cell $ toList cellSt where 
    elemBC_ cell_ [] = False
    elemBC_ cell_ (x:xs) | coord x == coord cell_ = True
                         | otherwise = elemBC_ cell_ xs

  notElemByCoord :: Cell -> CellStorage -> Bool
  notElemByCoord x y = not $ elemByCoord x y
