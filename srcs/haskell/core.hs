{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (Left, Right, floor)
import System.Environment
import System.Random
import Data.List
import Data.Maybe

-- defines and data structures
board_w :: Int
board_w = 10
board_h :: Int
board_h = 20

scores :: [Int]
scores = [0, 10, 30, 60, 100]

data Move = Down | Left | Right | Rotate_C | Rotate_C_C | Drop deriving (Enum)
instance Show Move where
  show = show . fromEnum

type Board = String
type Color = Int
data State = INIT_STATE | State
             {
               move :: Move,
               board :: Board,
               figIndex :: Int,
               rotateIndex :: Int,
               color :: Color,
               offsetX :: Int,
               offsetY :: Int,
               nextFigIndex :: Int,
               nextFigColor :: Color,
               score :: Int
             }
instance Show State where
  show s = intercalate " "
           [
             (show $ move s),
             (board s),
             (show $ figIndex s),
             (show $ rotateIndex s),
             (show $ color s),
             (show $ offsetX s),
             (show $ offsetY s),
             (show $ nextFigIndex s),
             (show $ nextFigColor s),
             (show $ score s)
           ]

type Coord = (Int, Int)
data Rotation = Rotation { squares :: [Coord], ofx :: Int, ofy :: Int }

figures :: [[Rotation]]
figures =
  [
    [ -- I
      Rotation { squares = [(0, 0), (1, 0), (2, 0), (3, 0)], ofx = 0, ofy = 2 },
      Rotation { squares = [(0, 0), (0, 1), (0, 2), (0, 3)], ofx = 2, ofy = 0 }
    ],
    [ -- L
      Rotation { squares = [(0, 0), (1, 0), (2, 0), (0, 1)], ofx = 0, ofy = 1 },
      Rotation { squares = [(0, 0), (1, 0), (1, 1), (1, 2)], ofx = 0, ofy = 0 },
      Rotation { squares = [(0, 1), (1, 1), (2, 1), (2, 0)], ofx = 0, ofy = 0 },
      Rotation { squares = [(0, 0), (0, 1), (0, 2), (1, 2)], ofx = 1, ofy = 0 }
    ],
    [ -- J
      Rotation { squares = [(0, 0), (1, 0), (2, 0), (2, 1)], ofx = 0, ofy = 1 },
      Rotation { squares = [(1, 0), (1, 1), (1, 2), (0, 2)], ofx = 0, ofy = 0 },
      Rotation { squares = [(0, 0), (0, 1), (1, 1), (2, 1)], ofx = 0, ofy = 0 },
      Rotation { squares = [(0, 0), (1, 0), (0, 1), (0, 2)], ofx = 1, ofy = 0 }
    ],
    [ -- S
      Rotation { squares = [(1, 0), (2, 0), (0, 1), (1, 1)], ofx = 0, ofy = 1 },
      Rotation { squares = [(0, 0), (0, 1), (1, 1), (1, 2)], ofx = 1, ofy = 0 }
    ],
    [ -- Z
      Rotation { squares = [(0, 0), (1, 0), (1, 1), (2, 1)], ofx = 0, ofy = 1 },
      Rotation { squares = [(0, 1), (1, 0), (1, 1), (0, 2)], ofx = 1, ofy = 0 }
    ],
    [ -- O
      Rotation { squares = [(0, 0), (0, 1), (1, 1), (1, 0)], ofx = 0, ofy = 0 }
    ],
    [ -- T
      Rotation { squares = [(0, 0), (1, 0), (2, 0), (1, 1)], ofx = 0, ofy = 1 },
      Rotation { squares = [(1, 0), (0, 1), (1, 1), (1, 2)], ofx = 0, ofy = 0 },
      Rotation { squares = [(1, 0), (0, 1), (1, 1), (2, 1)], ofx = 0, ofy = 0 },
      Rotation { squares = [(0, 0), (0, 1), (0, 2), (1, 1)], ofx = 1, ofy = 0 }
    ]
  ]

-- main
main :: IO ()
main = do
  args <- getArgs
  rg <- getStdGen
  let state = parseState args
  let nextState = update state rg
  if checkEndGame nextState
    then putStrLn "Game over!"
    else mapM_ putStrLn [show nextState, render nextState]

parseState :: [String] -> State
parseState args
  | (length args == 1 && args !! 0 == "INIT_STATE") = INIT_STATE
  | length args == 10 = State
                        (toEnum . read $ args !! 0)
                        (args !! 1)
                        (read $ args !! 2)
                        (read $ args !! 3)
                        (read $ args !! 4)
                        (read $ args !! 5)
                        (read $ args !! 6)
                        (read $ args !! 7)
                        (read $ args !! 8)
                        (read $ args !! 9)
  | otherwise = error "incorrect arguments"

-- update state logic
update :: State -> StdGen -> State
update INIT_STATE rg = State
                       Down
                       (concat $ replicate 200 "0")
                       figIndex
                       0
                       (fst $ randomR (1, 7) rg)
                       (if figIndex == 0 then 3 else 4)
                       offsetY
                       (fst $ randomR (0, length figures - 1) rg)
                       (fst $ randomR (1, 7) rg)
                       0
                       where figIndex = (fst $ randomR (0, length figures - 1) rg)
                             offsetY = -1 * ofy (figures !! figIndex !! 0)

update state@(State{..}) rg = case move of
  Down       -> if needNewFigure state
                then processNewFigure rg state
                else moveDown state
  Left       -> if canMoveLeft state
                then moveLeft state
                else state
  Right      -> if canMoveRight state
                then moveRight state
                else state
  Rotate_C   -> if canRotateClockwise state
                then state {rotateIndex = nextClockwiseIndex state}
                else state
  Rotate_C_C -> if canRotateCounterClockwise state
                then state {rotateIndex = nextCounterClockwiseIndex state}
                else state
  Drop       -> processNewFigure rg . dropFig $ state

checkEndGame :: State -> Bool
checkEndGame = not . canPlace

getFigCoords :: Int -> Int -> Int -> Int -> Maybe [Coord]
getFigCoords figIndex rotateIndex offsetX offsetY = if illegal coords
                                                    then Nothing
                                                    else Just ((filter (\(_, y) -> y >= 0)) coords)
  where illegal = any (\(x, y) -> y >= board_h || x < 0 || x >= board_w) :: [Coord] -> Bool
        coords = map (\(x, y) -> (x + offsetX + ofx f, y + offsetY + ofy f)) (squares f)
        f = figures !! figIndex !! rotateIndex

coordToInd :: Coord -> Int
coordToInd (x, y) = (x + board_w * y)

boardCellsFree :: Board -> [Coord] -> Bool
boardCellsFree board = all (\coord -> board !! coordToInd coord == '0')

canPlace :: State -> Bool
canPlace s = let coords = getFigCoords (figIndex s) (rotateIndex s) (offsetX s) (offsetY s)
             in case coords of
                  Just coords -> boardCellsFree (board s) coords
                  Nothing     -> False

moveDown :: State -> State
moveDown s@(State{..}) = s {offsetY = offsetY + 1}

moveLeft :: State -> State
moveLeft s@(State{..}) = s {offsetX = offsetX - 1}

moveRight :: State -> State
moveRight s@(State{..}) = s {offsetX = offsetX + 1}

needNewFigure :: State -> Bool
needNewFigure s = not $ canPlace $ moveDown s

createNewFig :: StdGen -> State -> State
createNewFig rg state@(State{..}) = State
                                    move
                                    board
                                    nextFigIndex
                                    0
                                    nextFigColor
                                    (if nextFigIndex == 0 then 3 else 4)
                                    (-1 * ofy (figures !! nextFigIndex !! 0))
                                    (fst $ randomR (0, 6) rg)
                                    (fst $ randomR (1, 7) rg)
                                    score

removeFullLines :: State -> State
removeFullLines s@(State{..}) = s {board = clear_board} {score = new_score}
  where full_lines = filter (\line -> any (\c -> c == '0') line) (chunksOf board_w board)
        clear_lines_count = board_h - length full_lines
        new_score = score + scores !! clear_lines_count
        clear_board = (replicate (board_w*clear_lines_count) '0' ++ concat full_lines)


paintSqToBoard :: Color -> Coord -> Board -> Board
paintSqToBoard color coord board = take (coordToInd coord) board ++ show color ++ drop (coordToInd coord + 1) board

addPieceToBoard :: State -> State
addPieceToBoard state@(State{..}) = state {board = new_board}
  where coords = getFigCoords figIndex rotateIndex offsetX offsetY
        new_board = foldr (paintSqToBoard color) board (fromJust coords)

processNewFigure :: StdGen -> State -> State
processNewFigure rg = createNewFig rg . removeFullLines . addPieceToBoard

canMoveLeft :: State -> Bool
canMoveLeft s = canPlace $ moveLeft s

canMoveRight :: State -> Bool
canMoveRight s = canPlace $ moveRight s

nextClockwiseIndex :: State -> Int
nextClockwiseIndex s = if (ind == len - 1) then 0 else ind + 1
  where ind = rotateIndex s
        len = length $ figures !! figIndex s

nextCounterClockwiseIndex :: State -> Int
nextCounterClockwiseIndex s = if (ind == 0) then len - 1 else ind - 1
  where ind = rotateIndex s
        len = length $ figures !! figIndex s

canRotateClockwise :: State -> Bool
canRotateClockwise s = canPlace (s {rotateIndex = nextClockwiseIndex s})

canRotateCounterClockwise :: State -> Bool
canRotateCounterClockwise s = canPlace (s {rotateIndex = nextCounterClockwiseIndex s})

dropFig :: State -> State
dropFig s =
  let lastBeforeDrop = takeWhile (not . needNewFigure) (iterate moveDown s)
  in case lastBeforeDrop of
       []   -> s
       list -> moveDown $ last list

-- render state logic
reset = "\x1B[m" -- reset escape sequence
inverse = "\x1B[7m" -- inverse white and black part of letter square
ceil = "\x2582"
floor = inverse ++ "\x2586" ++ reset
left = inverse ++ "\x258a" ++ reset
right = "\x258e"
spacer = "."
next_p_board_w = 6
next_p_board_h = 6

render :: State -> String
render s = intercalate "\n" $
           [" " ++ concat (replicate board_w ceil) ++ " "] ++
           zipWith (++) (renderBoardBody (board (addPieceToBoard s))) ((renderNextPieceBoard (score s) (nextFigColor s) (nextFigIndex s)) ++ repeat "") ++
           [" " ++ concat (replicate board_w floor) ++ " "]

renderBoardBody :: Board -> [String]
renderBoardBody board = map renderBoardRow (chunksOf board_w board)

renderBoardRow :: String -> String
renderBoardRow row = left ++ foldl (\acc ch -> acc ++ renderChar ch) "" (zip [0..] row) ++ right

-- replace board cell with color or spacer
renderChar :: (Int, Char) -> String
renderChar (n, c) = case (c, n `mod` 2) of
                       ('0', 0) -> " "
                       ('0', _) -> spacer
                       _        -> colors c ++ " " ++ reset

colors :: Char -> [Char]
colors c = case c of
  '0' -> ""         -- for empty
  '1' -> "\x1B[41m" -- BackgroundRed
  '2' -> "\x1B[42m" -- BackgroundGreen
  '3' -> "\x1B[43m" -- BackgroundYellow
  '4' -> "\x1B[44m" -- BackgroundBlue
  '5' -> "\x1B[45m" -- BackgroundMagenta
  '6' -> "\x1B[46m" -- BackgroundCyan
  '7' -> "\x1B[47m" -- BackgroundWhite

renderNextPieceBoard :: Int -> Color -> Int -> [String]
renderNextPieceBoard score color figIndex =
  [
    (" " ++ leftPad 6 '0' (show score)),
    (" " ++ concat (replicate next_p_board_w ceil) ++ " ")
  ] ++
  map renderNextPieceBoardRow (nextPieceBoardBody color figIndex) ++
  [(" " ++ concat (replicate next_p_board_w floor) ++ " ")]

renderNextPieceBoardRow :: String -> String
renderNextPieceBoardRow line = left ++ foldl (\acc ch -> acc ++ renderNextPieceBoardChar ch) "" line ++ right

renderNextPieceBoardChar :: Char -> String
renderNextPieceBoardChar c = case c of
                               ' ' -> [c]
                               _   -> colors c ++ " " ++ reset

nextPieceBoardBody :: Color -> Int -> [String]
nextPieceBoardBody nextFigColor nextFigIndex = foldr (paintSqToNextPieceBoard nextFigColor) emptyBoard (nextPieceCoords nextFigIndex)

emptyBoard :: [String]
emptyBoard = replicate next_p_board_h (replicate next_p_board_w ' ')

nextPieceCoords :: Int -> [Coord]
nextPieceCoords nextFigIndex = fromJust $ getFigCoords nextFigIndex 0 (if (nextFigIndex == 5) then 2 else 1) (if (nextFigIndex == 5) then 2 else 1)

paintSqToNextPieceBoard :: Color -> Coord -> [String] -> [String]
paintSqToNextPieceBoard color (x, y) board = take y board ++ [take x st ++ show color ++ drop (x+1) st] ++ drop (y+1) board
  where st = board !! y


-- utils
-- split list at given size chanks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n list = (take n list) : chunksOf n (drop n list)

-- pad string with given char
leftPad :: Int -> Char -> String -> String
leftPad n c str
  | length str < n = (replicate (n - length str) c) ++ str
  | otherwise      = str
