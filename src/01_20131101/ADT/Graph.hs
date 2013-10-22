module ADT.Graph
( Graph
, mkGraph
, adjacent
, nodes
, edgeIn
, weight
, edgesD
, edgesU
) where

import Data.Array

------ The Graph ADT

type Graph n w = Array (n, n) (Maybe w)

mkGraph :: (Ix n, Eq w) => Bool -> (n, n) -> [(n, n, w)] -> (Graph n w)
mkGraph direction bounds@(lower,upper) edges
    = emptyArray // ([((u, v), Just w) | (u, v, w) <- edges] ++
        if direction
            then []
            else [((v, u), Just w) | (u, v, w) <- edges, u /= v])
    where
        emptyArray = array ((lower, lower), (upper, upper))
            [((u, v), Nothing) | u <- range bounds, v <- range bounds]

adjacent :: (Ix n, Eq w) => (Graph n w) -> n -> [n]
adjacent g u = [v | v <- nodes g, (g!(u, v)) /= Nothing]

nodes :: (Ix n, Eq w) => (Graph n w) -> [n]
nodes g = range (lower, upper)
    where
        ((lower, _),(upper, _)) = bounds g
        -- bounds :: Ix i => Array i e -> (i, i)

edgeIn :: (Ix n, Eq w) => (Graph n w) -> (n, n) -> Bool
edgeIn g (u, v) = g!(u, v) /= Nothing

weight :: (Ix n, Eq w) => n -> n -> (Graph n w) -> w
weight u v g = w
    where
         (Just w) = g!(u, v)

edgesD :: (Ix n, Eq w) => (Graph n w) -> [(n, n, w)]
edgesD g = [(u, v, unwrap (g!(u, v)))
            | u <- nodes g, v <- nodes g, edgeIn g (u, v)]
    where
        unwrap (Just w) = w

edgesU :: (Ix n, Eq w) => (Graph n w) -> [(n, n, w)]
edgesU g = [(u, v, unwrap (g!(u, v)))
            | u <- nodes g, v <- range (u, upper), edgeIn g (u, v)]
    where
        (_, (upper, _)) = bounds g
        unwrap (Just w) = w
