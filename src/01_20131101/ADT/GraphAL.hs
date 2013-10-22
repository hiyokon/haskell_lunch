module ADT.GraphAL
( GraphAL
, mkGraphAL
, adjacentAL
, nodesAL
, edgeInAL
, weightAL
, edgesDAL
, edgesUAL
) where

import Data.Array

------ The Graph ADT

type GraphAL n w = Array n [(n, w)]

mkGraphAL :: (Ix n, Eq w) => Bool -> (n, n) -> [(n, n, w)] -> (GraphAL n w)
mkGraphAL direction bounds@(lower,upper) edges
    = accumArray (\xs x -> x:xs) [] bounds
        ([(u, (v, w)) | (u, v, w) <- edges] ++
            if direction
                then []
                else [(v, (u, w)) | (u, v, w) <- edges, u /= v])

adjacentAL :: (Ix n, Eq w) => (GraphAL n w) -> n -> [n]
adjacentAL g u = map fst (g!u)

nodesAL :: (Ix n, Eq w) => (GraphAL n w) -> [n]
nodesAL g = indices g

edgeInAL :: (Ix n, Eq w) => (GraphAL n w) -> (n, n) -> Bool
edgeInAL g (u, v) = elem v (adjacentAL g u)

weightAL :: (Ix n, Eq w) => n -> n -> (GraphAL n w) -> w
weightAL u v g = head [w | (v', w) <- g!u, (v' == v)]

edgesDAL :: (Ix n, Eq w) => (GraphAL n w) -> [(n, n, w)]
edgesDAL g = [(u, v, w) | u <- nodesAL g, (v, w) <- g!u]

edgesUAL :: (Ix n, Eq w) => (GraphAL n w) -> [(n, n, w)]
edgesUAL g = [(u, v, w) | u <- nodesAL g, (v, w) <- g!u, u < v]
