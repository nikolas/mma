type ScreenBuf = [ [Char] ]

class Shape a where
	-- is this point inside the shape?
	is_in :: a -> Point -> Bool

data Point = Point {
	x :: Int,
	y :: Int
	} deriving (Show)

{-
 -      b
 -     / \
 -    /   \
 -   /     \
 -  /       \
 - a---------c
 -}
data Triangle = Triangle {
	tri_pa :: Point,
	tri_pb :: Point,
	tri_pc :: Point
	} deriving (Show)

{-
 - a------------*
 - |            |
 - *------------b
 -}
data Rectangle = Rectangle {
	rect_pa :: Point,
	rect_pb :: Point
	} deriving (Show)

{-         
 -     _. ~ ._
 -    /       \
 -              
 -   (    a    )
 -             b
 -    \       /
 -     `~-_-~` 
 -}
data Circle = Circle {
	circ_pa :: Point,
	circ_pb :: Point
	} deriving (Show)

instance Shape Triangle where
	is_in (Triangle (Point x1 y1) (Point x2 y2) (Point x3 y3) )
		(Point a b) = True

instance Shape Rectangle where
	is_in ( Rectangle (Point x1 y1) (Point x2 y2) ) (Point x y) =
			(x >= x1 && x <= x2) && (y >= y1 && y <= y2)

instance Shape Circle where
	is_in ( Circle (Point x1 y1) (Point x2 y2) ) (Point x y) = True

-- blit a Shape to screen....
blit :: Shape a => a -> ScreenBuf
blit sh = [[ ch sh x y
	| x <- take screen_width [0 ..]]
	| y <- take screen_height [0 ..]]
	where
		ch sh x y = if is_in sh (Point x y) then 'r' else ' '

screen_width = 80
screen_height = 24

-- merge two ScreenBufs
screen_merge :: ScreenBuf -> ScreenBuf -> ScreenBuf
screen_merge x y = zipWith max x y

-- empty ScreenBuf
screen_buffer_empty :: ScreenBuf
screen_buffer_empty = replicate screen_height $ replicate screen_width ' '

-- print out screen_buffer
screen_update :: ScreenBuf -> IO ()
screen_update s = mapM_ putStrLn s

main :: IO ()
main = do
	let pa = Point 3 5
	let pb = Point 50 20
	screen_update $ screen_merge (blit (Rectangle pa pb)) (blit (Circle pa pb))

getPt :: String -> IO Point
getPt s = do
	numberString <- getLine
	let x = read numberString :: Int
	numberString <- getLine
	let y = read numberString :: Int
	return (Point x y)
