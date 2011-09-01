{-# LANGUAGE RecordWildCards , ParallelListComp #-}
-- Basic backend of GoL
-- Writes test based grid as follows 

{-
|X| | | | |
-----------
| | |X| | |
-----------
| |X| | | |
-----------
-}

module GoL.Backend.Txt where
  import Control.Exception
  import GoL.Definitions
  --import GoL.Backend.IF 
  import qualified Data.ByteString.Char8 as B
  import System.IO
  import Data.List
  import Control.Monad
  import GoL.Backend.IF

  ----------------------------------------------------
  --  Data Types 
  ----------------------------------------------------
  data GolTextBe = GolTextBe {
    generation :: !Int,
    array :: !B.ByteString,
    dim :: !Coord, -- Dimensions of grid
    fileName :: !FilePath
    -- Currently don't keep handle . Handle will be AD HOC , where file will be appended with each update
  } deriving (Eq)

   
  ----------------------------------------------------
  --  Functions
  ----------------------------------------------------
--  init :: Coord -> IO GolTextBe
--  init c@(x,y) = return $ fields where
--    fields = GolTextBe {..}
--    generation = 0 -- 
--    dim = c
--    array = B.pack $ replicate (x*y) ' ' -- Initial bytestring is array full of spaces

  -- Convert array in BE to bytestring that will show as Grid in text
  buildGrid :: GolTextBe -> B.ByteString
  buildGrid x = nice_grid where
    arr = array x
    x_dim = fst $ dim x
    y_dim = snd $ dim x
    lines = map B.unpack $ unfoldr slice_f arr where
      slice_f :: B.ByteString -> Maybe (B.ByteString,B.ByteString)
      slice_f jj | (B.null jj) = Nothing
                 | otherwise = Just $ B.splitAt x_dim jj

    h_sep = replicate x_dim '|' 
    v_sep_no_term =  replicate (x_dim*2+1) '-' 
    v_sep = v_sep_no_term ++ "\n"

    
    h_seped_lines = map (\ys -> join [[x]++[y] | x <- h_sep | y <- ys] ++ "|\n") lines  

    nice_grid = B.concat $ map B.pack  $  (join [[x] ++ [y] | x <- (replicate y_dim v_sep) | y <- h_seped_lines]) ++ [v_sep_no_term]

   
  -- Slice Bytestring to list of bytestring according to differential list 
  -- Not exactly fold, since fold for bytestrings is defined for Char as a base element and bytestring is storage 
  


--------------------------------------------------
  splitStr :: B.ByteString -> [Int]-> [B.ByteString]
      
  splitStr x lst =  (slc_') [] lst x where
  slc_' :: [B.ByteString]->[Int]->B.ByteString->[B.ByteString]
  slc_' acc [] next = acc ++ [next]
  slc_' acc (x:xs) src | (B.null src) = acc
                        | otherwise =  slc_' (acc++[curr]) xs  next where
                          (curr,next) = B.splitAt x src
--------------------------------------------------




    

   -- Update several indices in bytestring with new values
   -- Assuming indices is differential list then update will be series of take and then one concatenation

--------------------------------------------------
   -- Convert sorted list to diff list
  toDiffLst :: (Num a) => [a] -> [a]
  toDiffLst [] = []
  toDiffLst [x] = [x]
  toDiffLst lst@(x:xs) = [x] ++ zipWith (-) xs lst

   -- Update Byte String

--------------------------------------------------
-- Update bytestring with entries

-- Update Byte String
  updByteStr :: [(Int,Char)] -> B.ByteString -> B.ByteString
  updByteStr lst bstr = new_bstr where

    new_bstr =  B.concat $ concat $ transpose [slicedBstr,packedChr]  

    (intLst,charLst) = unzip lst
    
    diffIntLst :: [Int]
    diffIntLst = (toDiffLst . sort) intLst

    slicedBstr :: [B.ByteString]
    slicedBstr = splitStr bstr diffIntLst

    packedChr :: [B.ByteString]
    packedChr = map B.pack $ transpose [charLst]

--------------------------------------------------
        
   
  appAllChgBe :: GolTextBe -> [CellChange] -> GolTextBe    

  appAllChgBe be [] = GolTextBe { generation = generation be + 1,dim = dim be,fileName = fileName be,array = array be }

  appAllChgBe be chgLst = new_be where

    new_be = GolTextBe { generation = generation be + 1,dim = dim be,fileName = fileName be,array = new_array }

    (x_dim,y_dim) = dim be

    getPosCharPair :: CellChange -> (Int,Char)
    getPosCharPair (st,cl) = (cell_pos,_chr) where
      (x,y) = coord cl
      cell_pos = y * x_dim + x
      _chr = case st of
        Alive -> 'X'
        Dead  -> ' '
      
    pair_lst = map getPosCharPair chgLst  
    initBstr = array be
    new_array = updByteStr pair_lst initBstr

      
   


--------------------------------------------------


  instance GoLBackend GolTextBe where  

    --init :: GolTextBe -> Coord -> IO GolTextBe
    init base_const c@(x,y) = result where
      fields = GolTextBe {fileName = fileName_,..}
      fileName_ = fileName base_const
      generation = 0 -- 
      dim = c
      array = B.pack $ replicate (x*y) ' ' -- Initial bytestring is array full of spaces
      result = do
        writeFile (fileName_) ""
        return fields
     
    updateSt chgLst be = result where 
      next_be = appAllChgBe be chgLst
      result = do
        appendFile (fileName next_be) $ "\nGeneration #" ++ (show $ generation be) ++ "\n"
        appendFile (fileName next_be) "\nUpdate list is \n" 
        appendFile (fileName next_be) $ ( show chgLst ) ++ "\n"
        appendFile (fileName next_be) "\r\n"
        B.appendFile (fileName next_be) (buildGrid next_be)
        return next_be
