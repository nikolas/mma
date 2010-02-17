module InputState
	(MousePos(MousePos),
	 mouseX,
	 mouseY,
	 KeysState(KeysState),
	 leftKeyDown,
	 rightKeyDown,
	 downKeyDown,
	 upKeyDown,
	 leftMouseDown,
	 spaceKeyDown,
	 escKeyDown) where

data KeysState = KeysState
	{
		leftKeyDown     :: Bool,
		rightKeyDown    :: Bool,
		downKeyDown     :: Bool,
		upKeyDown       :: Bool,
		leftMouseDown   :: Bool,
		spaceKeyDown    :: Bool,
		escKeyDown      :: Bool
	}

data MousePos = MousePos
	{
		mouseX  :: Int,
		mouseY  :: Int
	}

