import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City, City, Distance)]

------------------------- Auxiliary Functions ----------------------------

-- Auxiliary function that checks if a Maybe value is Just or not.
checkMaybeValue :: Maybe a -> Bool
checkMaybeValue m = case m of
    Just _ -> True
    Nothing -> False

-- Auxiliary function to safely extract a value from a Maybe
extractMaybeValue :: Maybe a -> a
extractMaybeValue m = case m of
    Just m -> m 
    Nothing -> error "Nothing"

-- Auxiliary function for prelude compare function, used twice: once in Shortest Path and once in Travel Salesman
-- The `compare'` function takes a projection function and two elements.
-- It applies the projection function to both elements and compares the results.
-- This is useful for comparing elements based on a specific property.
compare' :: Ord b => (a -> b) -> a -> a -> Ordering
compare' projection x y = compare (projection x) (projection y)

-------------------------- Main Functions ---------------------------------

-- 1. cities
{- 
concatMap iterates over the road map for every tuple and picks the first two elements (the cities) while it 
concatenates the values in a list, then .nub removes any duplicate values, 
and .sort sorts the cities in order.
-}
cities :: RoadMap -> [City]
cities rm = Data.List.sort (Data.List.nub (concatMap (\(c1, c2, _) -> [c1, c2]) rm))

-- 2. areAdjacent
{- 
Iterates over tuples of the roadmap and checks if cities are adjacent. Null checks if the list is empty,
if so areAdjacent returns true (because of null), then not negates true and becomes false.
-}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = any (\(a, b, _) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm

-- 3. distance
{- 
Returns the distance between two cities if they are directly connected.
It filters the road map for a tuple where the cities match either way and returns the distance if found:
if a = c1 and b = c2 or vice versa, the condition is verified and the distance between the cities is returned
as a Just value.
-}
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = case filter (\(a, b, _) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm of
    [(_, _, d)] -> Just d
    _ -> Nothing

-- 4. adjacent
{-
This function returns a list of tuples containing cities and their corresponding distance to the city inserted initially.
The condition is correct if any city matches the city in the tuple.
-}
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent rm c = [(if c == c1 then c2 else c1, d) | (c1, c2, d) <- rm, c == c1 || c == c2]

-- 5. pathDistance 
{-
If the path is empty or only has one city the returned distance is 0.
-}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm [] = Just 0                        
pathDistance rm [_] = Just 0                        
pathDistance rm (c1:c2:cs) = do
    currentDist <- distance rm c1 c2
    rest <- pathDistance rm (c2:cs)
    return (currentDist + rest)

-- 6. rome :: RoadMap -> [City]
{-
In this function, we start by creating a list of tuples by mapping our list of cities in the road map
to a function that provides the degree of that city (length of list of adjacents).
Then, in highestDegree we use a map to find the max degree, which is the second element of the tuple.
Finally, we use a list comprehension to filter which cities have a degree that matches the Max Degree.
-}
rome :: RoadMap -> [City]
rome rm = [c | (c, degree) <- degrees, degree == highestDegree]
    where
        degrees = map (\c -> (c, length (adjacent rm c))) (cities rm)
        highestDegree = maximum (map snd degrees)

-- 7. isStronglyConnected :: RoadMap -> Bool
{-
We employ an all function to check if the condition inside the lambda function (verify is list
of strongly connected components equals the length of cites) is true or false.
To check if it's a scc, we apply a Depth First Search to our road map by visiting each city and their
adjacent cities one by one. If the city as already been visited we continue, otherwise
we add the city to the top of the list, and concatenate their adjacent cities to rest.
-}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = all (\c -> length (scc rm c) == length allCities) allCities
    where
        allCities = cities rm

scc :: RoadMap -> City -> [City]
scc rm start = dfs [start] []
    where
        dfs [] visited = visited
        dfs (c:cs) visited
            | c `elem` visited = dfs cs visited
            | otherwise = dfs (adjacentCities ++ cs) (c : visited)
            where
                adjacentCities = map fst (adjacent rm c)

-- 8. shortestPath :: RoadMap -> City -> City -> [Path]
{- 
The `shortestPath` function finds the shortest path between two cities in a RoadMap.
It uses Dijkstra's algorithm to calculate the shortest path from the starting city to the destination city.
The function returns a list of paths representing the route from the start city to the end city.
-}
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm start end = dijkstra rm start end

{-
The `dijkstra` function implements Dijkstra's algorithm to find the shortest path between two cities in a RoadMap.
It maintains a priority queue to explore cities based on the accumulated distance, always selecting the shortest distance first.
The search function processes each city, expanding its neighbors and updating the queue with possible paths.
The algorithm stops when it reaches the destination city, returning the path taken.
If the current city has already been visited, the function skips it to avoid processing it again.
The function returns an empty list if there is no valid path to the destination.
-}
dijkstra :: RoadMap -> City -> City -> [Path]
dijkstra rm start end = search [(0, [start])] [] 
    where
        search [] _ = []  
        search ((distance, current:restPath):rest) visited
            | current == end = [reverse (current:restPath)]  
            | current `elem` visited = search rest visited 
            | otherwise = search (Data.List.sortBy (compare' fst) (updateQueue rest)) (current : visited)
            where
                neighbors = [(c2, d) | (c1, c2, d) <- rm, c1 == current, c2 `notElem` visited]
                -- The `updateQueue` function takes a queue and updates it by folding over the list of neighbors.
                -- For each neighbor (next, d), it adds a new tuple to the accumulator `acc` with the updated distance and path.
                -- The new tuple consists of the sum of the current distance and `d`, and the updated path with `next` added to it.
                updateQueue queue = foldr (\(next, d) acc -> (distance + d, next : current : restPath) : acc) queue neighbors

-- 9. travelSales :: RoadMap -> Path
{-
The travelSales function aims to find the shortest possible round-trip path in a RoadMap that visits all cities exactly once and returns to the starting city. 
It generates all possible routes, filters out invalid paths, and then selects the shortest valid path. 
If there are no valid paths, it returns an empty list. 
The function uses helper functions to handle path permutations, distance calculation, and comparison.
-}
travelSales :: RoadMap -> Path
travelSales rm = case allCities of
    [] -> []  
    (startCity:rest) ->
        let
            cityPermutations = Data.List.permutations rest
            roundTripPaths = map (\perm -> startCity : perm ++ [startCity]) cityPermutations
            validPaths = filter (checkMaybeValue . pathDistance rm) roundTripPaths
            shortestPath = Data.List.minimumBy (compare' (extractMaybeValue . pathDistance rm)) validPaths
        in
            if null validPaths then [] else shortestPath
    where
        allCities = cities rm

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

