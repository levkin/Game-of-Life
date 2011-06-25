-- All backends will implement the same interface
--  



module GoL.Backend.IF where
  import GoL.Definitions

  -- Defining BE class
  -- Two backends -- Text and Java Gui
  -- In case of Text file . b is a array of ByteString 
  class GoLBackend b where
    -- Initializations .
    
    -- Basic initialization is size of grid
    init :: Coord -> IO b -- Initialize with size of grid

    -- Update state
    updateSt :: [CellChange] -> b -> IO b

