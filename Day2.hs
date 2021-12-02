module Day2 where
  
import qualified Data.Char as C

input = [
  "forward 5",
  "down 5",
  "forward 8",
  "up 3",
  "down 8",
  "forward 2"
  ]
  
-- Part 1

data Movement =
    Up Int
  | Down Int
  | Forward Int deriving (Show)
  
data Position =
  Position {
    horizontal :: Int
  , depth :: Int
  }
  deriving (Show)

toMovement :: String -> Movement
toMovement s =
  case words s of
    ["forward", num] -> Forward (stringToInt num)
    ["down", num] -> Down (stringToInt num)
    ["up", num] -> Up (negate . stringToInt $ num)
  where stringToInt s = read s :: Int

incDepth :: Int -> Position -> Position  
incDepth amount pos = Position (horizontal pos) (amount + depth pos)

incHorizontal :: Int -> Position -> Position
incHorizontal amount pos = Position (amount + horizontal pos) (depth pos)

zeroPosition = Position 0 0


move :: [Movement] -> Position
move ms =
  foldr (\m pos -> case m of
    Forward n -> incHorizontal n pos
    Up n      -> incDepth n pos
    Down n    -> incDepth n pos
  ) zeroPosition (reverse ms)
                       
             
finalDestination :: Position -> Int
finalDestination p = (horizontal p) * (depth p)

-- Part 2

move' :: [Movement] -> (Position, Int)
move' ms =
  foldr (\m (pos, aim) -> case m of
    Forward n -> (incDepth (n * aim) . incHorizontal n $ pos, aim)
    Up n      -> (pos, aim + n)
    Down n    -> (pos, aim + n)
  ) (zeroPosition, 0) (reverse ms)

finalDestination' :: (Position, Int) -> Int
finalDestination' (p, _) = (horizontal p) * (depth p)
