module Main where
import System.Environment
import Data.List

getLines :: Int -> IO [String]
getLines 1 = do
  s <- getLine
  return [s]
getLines count = do
  s1 <- getLine
  s2 <- getLines (count - 1)
  return (s1:s2)

getArrArray :: [String] -> [[Int]]
getArrArray = map (map (\ x -> read x :: Int) . words)

getEdgesFromRow :: [Int] -> Int -> [Int]
getEdgesFromRow [] _ = []
getEdgesFromRow (rowElement:row)  columnIndex
  | rowElement == 1 = columnIndex:getEdgesFromRow row (columnIndex+1)
  | rowElement == 0 = getEdgesFromRow row (columnIndex+1)
  | otherwise = []

getGraph :: [[Int]] -> [[Int]]
getGraph arrArray = do
  map (`getEdgesFromRow` 0) arrArray

setColor :: Int -> Int -> (Int -> Int) -> (Int -> Int)
setColor setVertice color colorFunction vertice
  | setVertice == vertice = color
  | otherwise = colorFunction vertice


visitNeighs  :: [[Int]] -> [Int] -> (Int -> Int) -> (Bool, Int -> Int)
visitNeighs _ [] colorFunction = (True, colorFunction)
visitNeighs graph (neigh:neighs) colorFunction = do
  let (result, newColorFunction) = dsuBipartite graph neigh colorFunction in
    if not result then (False, newColorFunction) else
      let (secondResult, newColorFunction2) = visitNeighs graph neighs newColorFunction in
        (result && secondResult, newColorFunction2)

dsuBipartite :: [[Int]] -> Int -> (Int -> Int) -> (Bool, Int -> Int)
dsuBipartite graph currentVertice colorFunction = do
  let currentColor = colorFunction currentVertice in
    let graphVerticeNeighs = (graph!!currentVertice) in
      let isBipartite = foldl (\x y -> x && (colorFunction y /= currentColor)) True (filter (\x -> colorFunction x /= 0) graphVerticeNeighs) in
        let neighs = filter (\x -> colorFunction x == 0) graphVerticeNeighs in
          let newColorFunction = foldl (\cf y -> setColor y (currentColor*(-1)) cf) colorFunction neighs in
            if isBipartite then visitNeighs graph neighs newColorFunction else (False, newColorFunction)

beginColorFunction :: Int -> Int
beginColorFunction x
  | x == 0 = 1
  | otherwise = 0

main :: IO ()
main = do
  args <- getArgs
  let file_name = head args
  fl <- readFile file_name
  let fl_lines = lines fl
  let q = read (fl_lines!!0)::Int
  let arrDrop = tail fl_lines
  let fn = file_name
  let x = getArrArray arrDrop
  let (result, function) = (dsuBipartite (getGraph x) 0 beginColorFunction)
  print result


