type Node = Int
type Edge = (Node, Node)
type Graph = [(Node, [Node])]

allPaths :: Graph -> Node -> Node -> [[Node]]
allPaths graph start end = findPaths graph start end []

findPaths :: Graph -> Node -> Node -> [Node] -> [[Node]]
findPaths graph current end visited
    | current == end = [reverse (current:visited)]
    | otherwise =
        let neighbors = filter (`notElem` visited) (findNeighbors graph current)
            paths = concatMap (\neighbor -> findPaths graph neighbor end (current:visited)) neighbors
        in paths

findNeighbors :: Graph -> Node -> [Node]
findNeighbors graph node = case lookup node graph of
    Just neighbors -> neighbors
    Nothing -> []

main :: IO ()
main = do
  let
    exampleGraph = [(1, [2, 3, 4]), (2, [1, 3, 4]), (3, [1, 2, 4]), (4, [1, 2, 3])]
  print (allPaths exampleGraph 1 4)