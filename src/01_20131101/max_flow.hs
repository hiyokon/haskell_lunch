import Data.Array
import ADT.Graph
import ADT.GraphAL
import ADT.Stack

---- Main

------ biGraph = mkGraph True (1, 11) (adListToAdMatrix inputData)

adListToAdMatrix :: [(u, [(v, w)])] -> [((u, v), w)]
adListToAdMatrix []           = []
adListToAdMatrix (ls:lss) = (former ls) ++ (adListToAdMatrix lss)
    where
        former (u, [])     = []
        former (u, vw:vws) = ((u, fst vw), snd vw) : former (u, vws)

adMatrixToAdList :: [((u, v), w)] -> [(u, [(v, w)])]
adMatrixToAdList []           = []
adMatrixToAdList (ms:mss) = ls : (adMatrixToAdList mss)
    where
        ((u, v), w) = ms
        ls = (u, [(v, w)])

-- mkBiGraph kids candies = mkGraph True (1, m + n) edges
--     where
--         m = length kids
--         n = length candies
--         edges = [(from, to)
--                 | from <- range (1, m)
--                 , to <- range (m + 1, m + n)
--                 , ]

-- mkSourceEdges :: (Ix n, Eq w) => [Kid] -> Graph n w
-- mkSourceEdges kids = mkGraph True (0, n) edges
--     where
--         n = length kids
--         edges = [(0, kid) | kid <- kids]

-- mkNetwork :: (Ix n, Eq w) => [w] -> [w] -> (Graph n w) -> (Graph n w)
-- mkNetwork sourceFlows targetFlows bigraph
--     = mkGraph True (source, target) (sourceEdges ++ bgEdges ++ targetEdges)
--     where
--         bgEdges = edgesD bigraph
--         ((lower, _), (upper, _)) = bounds bigraph
--         source = lower - 1
--         target   = upper + 1
--         sourceEdges = [(source, m, f)
--                       | (m, f) <- zip [lower..(lower + length sourceFlows)] sourceFlows]
--         targetEdges   = [ (m, target, f)
--                       | (m, f) <- zip [(upper - length targetFlows)..upper] targetFlows]
-- 

------ Test Data

---- Memo

-- type Graph n w = [(n,[n,w])]
-- type Graph = ([Node],[Edge])
-- tyep BiGraph = (Nodes, Nodes, Edges)
-- type Node = (Ix, NodeValue)
-- type NodeValue = Int
-- type Edge = (Node, Node, EdgeValue)
-- type EdgeValue = (Capacity, Cost)
-- type Capacity = Int
-- type Cost = Int

{-
- 抽象的な G (f,V,E) は、後で変換する関数を用意してあげればいいので、
- まずは抽象度の低い隣接行列から (計算上はこちらのほうが都合が良い)
- そのためには、行列の準備、これも多次元配列によって定義する
- 関数型プログラミングとは抽象化することと見つけたり
- ちなみに Haskell で行列を使うときは hmatrix 便利そう
-}


-- type Matrix   = [[Element]]
-- type Element  = (Capacity, Cost)
-- type Capacity = Int
-- type Cost     = Int
-- 
-- initMatrix :: Int -> Int -> Matrix
-- initMatrix row col = take row $ repeat $ take col $ repeat initElm
--     where
--         initElm = (0, 0)
-- 
-- getElement :: Int -> Int -> Matrix -> Element
-- getElement row col mat = mat !! row !! col
-- 
-- setElement :: Int -> Int -> Element -> Matrix -> Matrix
-- setElement row col elm mat = pre ++ ([elm] : pst)
--         where
--             pre = take (row * col + col    ) mat
--             pst = drop (row * col + col + 1) mat
-- 

-- type Matrix n w = Array ((n, n),(n, n)) w
-- mkMatrix m n elm = array ((1,1),(m,n)) [((i,j), elm) | i <- [1..m], j <- [1..n]]

-- {Depth,Breadth}-first search
--
-- depthFirstSearch :: Ix a => a -> Graph a -> [a]
-- depthFirstSearch start g = reverse (dfs [start] [])
--     where
--         dfs [] visited = visited
--         dfs (c:cs) visited
--             | elem c visited = dfs cs visited
--             | otherwise = dfs ((adjacent g c) ++ cs) (c : visited)
--
{-
data Kid = Kid
         { name :: String
         , favorites :: [(String, Int)]
         } deriving (Eq, Show, Read)

data Candy = Candy
           { kind :: String
           , quantity :: Int
           } deriving (Eq, Show, Read)

girls = [ Kid { name = "madoka", favorites = [("tea",400),("peach" ,800)] }
        , Kid { name = "sayaka", favorites = [("tea",200),("coffee",500)] }
        , Kid { name = "homura", favorites = [("tea",400),("melon" ,900)] }
        , Kid { name = "kyoko" , favorites = [("tea",400),("apple" ,100)] }
        , Kid { name = "mami"  , favorites = [("tea",400),("melon" ,100)] }
        ]

candies = [ Candy { kind = "peach" , quantity = 2 }
          , Candy { kind = "soda"  , quantity = 1 }
          , Candy { kind = "coffee", quantity = 2 }
          , Candy { kind = "apple" , quantity = 4 }
          , Candy { kind = "tea"   , quantity = 1 }
          , Candy { kind = "melon" , quantity = 3 }
          ]

mkTriple :: a -> (b, c) -> (a, b, c)
mkTriple a (b, c) = (a, b, c)

mkTriples :: [Kid] -> [(String, String, Int)]
mkTriples [] = []
mkTriples (kid:kids) = triples ++ mkTriples kids
    where
        triples = map (mkTriple $ name kid) (favorites kid)

graphTest = mkGraph True (1,5) [(1,2,(1,3,4))
                                ,(1,3,(1,3,4))
                                ,(1,4,(1,3,4))
                                ]
-}

