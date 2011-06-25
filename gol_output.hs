-- Implements XL backend
-- Cells are marked with coloured background . Current state of cells is dumped as coma separated files
-- A marks alive cell

module GoL.Output.XL where
  
  import Control.Monad.ST 
  import Data.Array.ST  
  
  ----------------------------------------------------
  --  Data definitions
  ----------------------------------------------------

  _XSize = 20
  _YSize = 20

