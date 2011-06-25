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

  ----------------------------------------------------
  --  Data Types 
  ----------------------------------------------------
  data GolTextBe = GolTextBe {
    generation :: !Int,
    array :: !B.ByteString,
    dim :: !Coord -- Dimensions of grid
  } deriving (Eq)

  ----------------------------------------------------
  --  Functions
  ----------------------------------------------------
  init :: Coord -> IO GolTextBe
  init c@(x,y) = return $ fields where
    fields = GolTextBe {..}
    generation = 0 -- 
    dim = c
    array = B.pack $ replicate (x*y) ' ' -- Initial bytestring is array full of spaces

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

  
    
