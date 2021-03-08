module Main where
import System.Environment

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

getRightNeighbor :: [[Int]] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getRightNeighbor arrArray (r, c) h w= do
  let arrVal = arrArray !! r !! (c+1) in
    [(r, c+1) | c /= (w-1) && arrVal `elem` [0,-2]]


getLeftNeighbor :: [[Int]] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getLeftNeighbor arrArray (r, c) h w = do
  let arrVal = arrArray !! r !! (c-1) in
    [(r, c-1) | c /= 0 && arrVal `elem` [0,-2]] ++ getRightNeighbor arrArray (r,c) h w


getBotNeighbor :: [[Int]] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getBotNeighbor arrArray (r, c) h w= do
  let arrVal = arrArray !! (r+1) !! c in
    [(r+1, c) | r /= (h-1) && arrVal `elem` [0,-2]] ++ getLeftNeighbor arrArray (r,c) h w

getTopNeighbor :: [[Int]] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getTopNeighbor arrArray (r, c) h w = do
  let arrVal = arrArray !! (r-1) !! c in
    [(r-1, c) | r /= 0 && arrVal `elem` [0,-2]] ++ getBotNeighbor arrArray (r,c) h w

getNeighbors :: [[Int]] -> (Int, Int) -> Int -> Int -> [(Int, Int)]
getNeighbors = getTopNeighbor

replaceListElement :: [a] -> Int -> a -> [a]
replaceListElement arr index val =
  take index arr ++ (val:drop (index+1) arr)

calculateWay :: (Int, Int) -> (Int, Int) -> Int
calculateWay (r_from, c_from) (r_to, c_to) = case (r_to-r_from, c_to-c_from) of
  (1, 0) -> 2
  (-1, 0) -> 3
  (0, 1) -> 4
  (0, -1) -> 5
  (_, _) -> -1

setEdge:: [[Int]] -> (Int, Int) -> Int -> [[Int]]
setEdge arrArray (r, c) val = replaceListElement arrArray r (replaceListElement (arrArray !! r) c val)

visitEdge :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int -> Int -> [[Int]]
visitEdge arrArray (r_from, c_from) (r_to, c_to) h w = do
  let way = calculateWay (r_from, c_from) (r_to, c_to) in
    setEdge arrArray (r_to, c_to) way

bsf :: [[Int]] -> (Int, Int) -> Int -> Int -> [[Int]]
bsf graph start = bsfIter graph [start]

visitEdgesFrom :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Int -> Int -> [[Int]]
visitEdgesFrom g _ [] _ _ = g
visitEdgesFrom graph from edges h w = do
  let edge = head edges in
      visitEdgesFrom (visitEdge graph from edge h w) from (tail edges) h w

getWayBackById :: Int -> (Int, Int)
getWayBackById id = case id of
  2 -> (-1,0)
  3 -> (1, 0)
  4 -> (0, -1)
  5 -> (0, 1)
  _ -> (0, 0) -- impossible except IO failure

bsfIter :: [[Int]] -> [(Int, Int)] -> Int -> Int  -> [[Int]]
bsfIter arr queue h w = do
  let (r,c) = head queue in
    let ret = tail queue in
      let neighs = getNeighbors arr (r, c) h w in
        let graphUpdated = visitEdgesFrom arr (r, c) neighs h w in
          let newQueue = (ret ++ neighs)in
            if null newQueue then arr else bsfIter graphUpdated newQueue h w

join :: String -> [String] -> String
join sep = foldr (\ a b -> a ++ if b == "" then b else sep ++ b) ""

formatGraph :: [[Int]] -> String
formatGraph g = do {
  join "\n" (map (join " " . map show) g)
  }

getPathByWay :: [[Int]] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getPathByWay graph (r_c, c_c) find path = do {
  if (r_c, c_c) == find then (r_c, c_c):path else do {
    let (way_r, way_c) = getWayBackById (graph!!r_c!!c_c) in
    getPathByWay graph (r_c+way_r, c_c+way_c) find ((r_c, c_c):path)
    }
  }

getEdgeFromString :: String -> (Int, Int)
getEdgeFromString str = do {
  let ws = words str in
    ((read (head ws)::Int) - 1, (read (ws!!1)::Int) - 1)
  }

main :: IO ()
main = do
 args <- getArgs
 let file_name = head args
 fl <- readFile file_name
 let fl_lines = lines fl
 let h_int = read (fl_lines!!0)::Int
 let w_int = read (fl_lines!!1)::Int
 let arrDrop = drop 2 fl_lines
 let arr = take h_int arrDrop
 let arrDrop2 = drop h_int arrDrop
 let start = getEdgeFromString (arrDrop2!!0)
 let (r_end, c_end) = getEdgeFromString (arrDrop2!!1)
 let fn = file_name in
  let x = getArrArray arr in
     let bsfGraph = bsf (setEdge x start (-1)) start h_int w_int in
     writeFile (fn ++ ".result") $ if bsfGraph!!r_end!!c_end == 0 then "N" else do {
      "Y\n" ++ do {
        join "\n" (map (\(x, y) -> join " " [show x, show y]) (map (\(x, y) -> (x+1, y+1))(getPathByWay bsfGraph (r_end, c_end) start [])))
        }
     }
  




