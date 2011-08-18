{-# LANGUAGE RecordWildCards , NamedFieldPuns #-}

module GoL.Game where
  import GoL.Generation
  import GoL.Definitions
  import GoL.Backend.IF

  -- Given backend , Initial cell configuration and number of turns -> Play the game
  -- Game played -> New state of BE + new cell storage . Game may be applied multiple times
  -- I'd like to have game ... >>= game 
   
   -- Turn stream 
  data Turn = Last | Continue Turn

  
  data (GoLBackend a) => GameState a = GameState {
    cellStorage :: CellStorage,
    backEnd :: a
  }


  game :: (GoLBackend a) => Turn -> (GameState a) -> IO (GameState a)
  
  -- If this is last turn -> Don't do anything
  game Last st = return st 

  game (Continue nxt_turn) state@(GameState storage be) = do
    let (changes,new_storage) = simulate_generation storage
    new_be <- updateSt changes be  
    game nxt_turn (GameState new_storage new_be) 
    
