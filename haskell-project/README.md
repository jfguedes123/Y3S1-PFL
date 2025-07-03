# PFL - Projeto Haskell

## T13 G10

### Group Members

| Student            | ID         | Contribution |
|--------------------|------------|--------------|
| João Guedes         | up202108711 | 50%          |
| Eduardo Machado    | up202105337 | 50%          |

We collaborated on every single implementation and function, there was no real division of work. Except for the last two functions, where Eduardo Machado focused more on the shortestPath and João Guedes focused more on travelSales.

### Shortest Path Implementation

For our implementation of shortestPath, we opted to implement the Dijkstra algorithm. Based on previous university courses, we knew that to find the shortest path, Dijkstra was a very efficient algorithm to use in graphs with non-negative weights with a time complexity of O(|E| + |V|log|V|). The way our program works is by intializing the queue with the start city and a distance of 0. The function processes each city by selecting the city with the smallest distance in our queue (they are ordered from smallest distance to largest). We call expandQueue to simulate the behaviour of a priorityQueue. We update the possible paths by adding the neighbours of the previously visited city in order of smallest to largest distance (these distances are updated with the accumulated distance), and marking the previous city as visited. This way we ensure that we don't look at previously visited cities. Once the destination is reached, the program ends and we have our shortest path.

### Travel Sales Implementation

For our implementation of travelSales to solve the Traveling Salesman Problem for the given Road Map, we implemented a brute force algorithm. We do this by creating all possible permutations for possible all possible paths; all of these permutations are added to our list: cityPermutations. In roundTripPaths, we add our starting City to the beginning and the end of each path, this way we ensure that we test the full round-trip of the TSP algorithm. Since we are creating every possible path, there's obviously going to be impossible paths in our list, so we created a filter that filters out every invalid path by checking if the value of their distance comes out as a Just value or Nothing. Finally, we return our shortest path using a Prelude function: minimumBy. As TSP algorithm is a np-hard problem, we decided to implement this brute force solution which works fine for the small data that we tested on (gTest1, 2, 3), however for larger data sets it grows in permutations and becomes computationally expensive.

### Additional Tests: QuickCheck

Based on a suggestion given by our theory class' professor, we decided to create some properties to test if our code passed every test using QuickTest/Cabal. During the making of these tests, we notice that the tests for traveSales kept either failing or stopping, this is do to the increasing factorial complexity of generating permutations, which the code couldn't compute, that why we adjust the limit of cities that we allowed for testing.

```hs
--------------- Tests -----------------

-- Property for cities function
prop_cities :: RoadMap -> Bool
prop_cities rm = all (`elem` allCities) uniqueCities && all (`elem` uniqueCities) allCities
  where
    uniqueCities = cities rm
    allCities = Data.List.nub (concatMap (\(c1, c2, _) -> [c1, c2]) rm)

-- Property for areAdjacent function
prop_areAdjacent :: RoadMap -> City -> City -> Bool
prop_areAdjacent rm c1 c2 = areAdjacent rm c1 c2 == any (\(a, b, _) -> (a == c1 && b == c2) || (a == c2 && b == c1)) rm

-- Property for distance function
prop_distance :: RoadMap -> City -> City -> Bool
prop_distance rm c1 c2 = case distance rm c1 c2 of
    Just d -> any (\(a, b, dist) -> (a == c1 && b == c2 || a == c2 && b == c1) && dist == d) rm
    Nothing -> not (areAdjacent rm c1 c2)

-- Property for adjacent function
prop_adjacent :: RoadMap -> City -> Bool
prop_adjacent rm c = all (\(neighbor, d) -> areAdjacent rm c neighbor && checkDistance d) (adjacent rm c)
  where
    checkDistance d = any (\(a, b, dist) -> (a == c && dist == d) || (b == c && dist == d)) rm

-- Property for pathDistance function
prop_pathDistance :: RoadMap -> Path -> Bool
prop_pathDistance rm path = case pathDistance rm path of
    Just d -> d >= 0
    Nothing -> any (\(c1, c2) -> not (areAdjacent rm c1 c2)) (zip path (tail path))

-- Property for rome function
prop_rome :: RoadMap -> Bool
prop_rome rm = all (\c -> length (adjacent rm c) == maxDegree) (rome rm)
  where
    degrees = map (\c -> (c, length (adjacent rm c))) (cities rm)
    maxDegree = maximum (map snd degrees)

-- Property for isStronglyConnected function
prop_isStronglyConnected :: RoadMap -> Bool
prop_isStronglyConnected rm = isStronglyConnected rm == all (\c -> length (scc rm c) == length (cities rm)) (cities rm)

-- Property for shortestPath function
prop_shortestPath :: RoadMap -> City -> City -> Bool
prop_shortestPath rm start end
    | not (start `elem` allCities && end `elem` allCities) = True 
    | null allCities = True 
    | start == end = null paths || (not (null paths) && head (head paths) == start && last (head paths) == end)
    | otherwise = case paths of
        [] -> not (isStronglyConnected rm)
        (p:_) -> head p == start && last p == end && all (`elem` allCities) p
  where
    allCities = cities rm
    paths = shortestPath rm start end

-- Property for travelSales function with a limit on the number of cities
prop_travelSales :: RoadMap -> Bool
prop_travelSales rm
    | length (cities rm) > 8 = True -- Skip test if more than 8 cities to avoid long computation
    | otherwise = case travelSales rm of
        [] -> length (cities rm) <= 1 || not (isStronglyConnected rm)
        path -> length path == length (cities rm) + 1 && head path == last path && all (`elem` cities rm) (init path)


-- Run all the tests
main :: IO ()
main = do
    quickCheck prop_cities
    quickCheck prop_areAdjacent
    quickCheck prop_distance
    quickCheck prop_adjacent
    quickCheck prop_pathDistance
    quickCheck prop_rome
    quickCheck prop_isStronglyConnected
    quickCheck prop_shortestPath
    quickCheck prop_travelSales
```

