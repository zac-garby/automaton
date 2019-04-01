module Automaton
    ( Rule
    , Pos
    , Palette
    , Board
    , newBoard
    , fromList
    , width
    , height
    , size
    , wrap
    , at
    , step
    , neighbours
    , countNeighbours
    , count
    , gol
    , checkerboard
    , random
    , option
    , steps
    , nthStep
    , renderBoard
    ) where

import           Data.List
import qualified Data.Vector as V
import           Data.Vector (Vector, (!))
import           Control.Monad
import           System.Random (randomRIO, Random)
import           Graphics.GD

type Rule a = Pos -> Board a -> a -> a
type Pos = (Int, Int)
type Palette a = a -> Color
type Board a = Vector (Vector a)

newBoard :: Int -> Int -> (Pos -> a) -> Board a
newBoard width height gen = V.generate height $ \y -> V.generate width $ \x -> gen (x, y)

fromList :: [[a]] -> Board a
fromList = V.fromList . (map V.fromList)

width :: Board a -> Int
width = V.length . V.head

height :: Board a -> Int
height = V.length

size :: Board a -> (Int, Int)
size = (,) <$> width <*> height

wrap :: Pos -> Board a -> Pos
wrap (x, y) board = (x', y')
    where x' = if x < 0 then x + (width board) else if x >= (width board) then x - (width board) else x
          y' = if y < 0 then y + (height board) else if y >= (height board) then y - (height board) else y

at :: Board a -> Pos -> a
board `at` pos = board ! y ! x
    where (x, y) = wrap pos board

step :: Rule a -> Board a -> Board a
step rule board = newBoard w h $ \pos -> rule pos board (board `at` pos)
    where (w, h) = size board

neighbours :: Board a -> Pos -> [a]
neighbours board (x, y) = map (board `at`)
    [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

countNeighbours :: (Eq a) => (a -> Bool) -> Board a -> Pos -> Int
countNeighbours check board pos = count check $ neighbours board pos

count :: (Eq a, Foldable t) => (a -> Bool) -> t a -> Int
count pred = foldl foldingFunction 0
    where foldingFunction total val = if pred val then total + 1 else total

gol :: Rule Bool
gol pos board True = let n = countNeighbours id board pos in n == 2 || n == 3
gol pos board False = let n = countNeighbours id board pos in n == 3

checkerboard :: Int -> Int -> a -> a -> Board a
checkerboard width height a b = newBoard width height $ \(x, y) ->
    if even $ x + y then a else b

random :: (Random a) => Int -> Int -> (a, a) -> IO (Board a)
random width height range = do
    contents <- replicateM height $ replicateM width $ randomRIO range
    return $ fromList contents

option :: a -> a -> (Bool -> a)
option t f True = t
option t f False = f

steps :: Rule a -> Board a -> [Board a]
steps = iterate . step

nthStep :: Int -> Rule a -> Board a -> Board a
nthStep n rule = last . take n . iterate (step rule)

renderBoard :: (Show a) => Palette a -> Board a -> IO Image
renderBoard palette board = do
    let boardSize@(w, h) = size board
    img <- newImage boardSize
    forM_ [ (x, y) | x <- [1..w-2], y <- [1..h-2] ] $ \pos -> setPixel pos (palette $ board `at` pos) img
    return img
