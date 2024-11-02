import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.
type RoadMap = [(City,City,Distance)]
type City = String
type Path = [City]
type Distance = Int
type CitiesDistanceMatrix = Data.Array.Array (Int, Int) (Maybe Distance)
type AdjacentCities =  [(City,[(City,Distance)])]
type Bit = Integer



--- 1. Implement the function cities :: RoadMap -> [City] that returns the list of cities in the roadmap, without repetitions. ---
--- Note: you can use the function nub from Data.List to remove repetitions from a list. ---
--- Note: you can use the function concatMap from Data.List to concatenate the lists of cities from the tuples of the roadmap. ---
cities :: RoadMap -> [City]
cities ourRoadMap = Data.List.nub $ concatMap (\(a,b,_) -> [a,b]) ourRoadMap

--- 2. Implement the function areAdjacent :: RoadMap -> City -> City -> Bool that returns True if the two cities are adjacent in the roadmap, and False otherwise. ---
--- Note: you can use the function any from to check if there is any tuple in the roadmap that has the two cities. ---
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent ourRoadMap city1 city2 = any (\(startCity, endCity, _) -> (city1 == startCity && city2 == endCity) || (city1 == endCity && city2 == startCity)) ourRoadMap 

--- 3. Implement the function distance :: RoadMap -> City -> City -> Maybe Distance that returns the distance between two cities in the roadmap, if they are adjacent, and Nothing otherwise. ---
--- Note: you can use the function filter to get the tuples that have the two cities. ---
--- Note: you can use pattern matching to get the distance from the tuple. ---
distance :: RoadMap -> City -> City -> Maybe Distance
distance ourRoadMap city1 city2 = case filter (\(startCity, endCity, _) -> (city1 == startCity && city2 == endCity) || (city1 == endCity && city2 == startCity)) ourRoadMap of
    [] -> Nothing
    [(_, _, d)] -> Just d

--- 4. Implement the function adjacent :: RoadMap -> City -> [(City,Distance)] that returns the list of cities adjacent to a given city, with the respective distances. ---
--- Note: you can use the function map to transform the tuples of the roadmap into tuples with the city and the distance. ---
--- Note: you can use the function filter to get the tuples that have the city. ---
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent ourRoadMap city1 = (map (\(startCity,endCity, d) -> if startCity == city1 then (endCity, d) else (startCity, d)) . filter (\(startCity, endCity, _) -> (city1 == startCity) || (city1 == endCity))) ourRoadMap

--- 5. Implement the function pathDistance :: RoadMap -> Path -> Distance that returns the total distance of a currentPath in the roadmap. ---
--- Note: you can use the function zip to get the pairs of cities in the currentPath. ---
--- Note: you can use the distance function to get the distance between the city and the adjacent city. ---
--- Note: you can use the function map to get the distances between the pairs of cities. ---
--- Note: you can use the function sum to get the total distance of the currentPath. ---
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance ourRoadMap ourPath = case ourPath of
    [] -> Just 0
    [city] -> Just 0
    (city1:city2:remainingPath) -> case distance ourRoadMap city1 city2 of
        Just d -> case pathDistance ourRoadMap (city2:remainingPath) of
            Just d2 -> Just (d + d2)
            Nothing -> Nothing
        Nothing -> Nothing

--- 6. Implement the function rome :: RoadMap -> [City] that returns a list of the cities with the most amount of other cities connected. ---
--- Note: you can use the function map to create a list of tuples containing the city itself as the first element and the degree of the city (number of roads connected to that city) as the second element. ---
--- Note: you can use the function filter in conjunction with the lambda function (checks whether the current city is present in either position of the tuple given by our roadmap), to get a list of all the tuples in our roadmap that involve the current city. ---
--- Note: you can use the function length to get the length of the list produced by the function filter. ---
--- Note: you can use the function cities to get a list of all the cities without repetition. ---
--- Note: you can use the function map in conjunction with the function snd to get the second element of the tuple (represents each city's number of road connections).---
--- Note: you can use the function maximum to get the maximum degree (highest number of roads connected). ---
--- Note: you can use the function map in conjunction with the function fst and the function filter to get the first element of the tuple (city name) with the number of road connections matching the maximum number of road connections. ---
rome :: RoadMap -> [City]
rome ourRoadMap =
    let cityDegrees = map (\city -> (city, length $ filter (\(startCity, endCity, _) -> city == startCity || city == endCity) ourRoadMap)) $ cities ourRoadMap 
    in map fst $ filter (\(_, degree) -> degree == maximum (map snd cityDegrees)) cityDegrees

--- 7. Implement the function isStronglyConnected :: RoadMap -> Bool that returns True if all the cities in the roadmap are connected to eachother and False otherwise. ---
--- Note: you can use the function elem to check if the current city is already visited, if True no more cities are reachable and the list of visited cities is returned (BASE CASE). ---
--- Note: you can use the function foldl to add the current city to the list of visited cities and then recursively call dfs on each neighbor. ---
--- Note: (\vis (nextCity, _) -> dfs ourRoadMap nextCity vis) : applies dfs to the city connected to the current city. ---
--- Note: (curCity : visCities) : adds current city to the list of visited cities. ---
--- Note: (adjacent ourRoadMap curCity) : using adjacent function (defined above) to get a list of cities directly connected to the current city. ---
dfs :: RoadMap -> City -> [City] -> [City]
dfs ourRoadMap curCity visCities
    | curCity `elem` visCities = visCities
    | otherwise = foldl (\vis (nextCity, _) -> dfs ourRoadMap nextCity vis)
                        (curCity : visCities)
                        (adjacent ourRoadMap curCity)

--- Note: you can use the function cities to get a list of all the cities without repetition. ---
--- Note: you can use the function head to get the first city of the list (Since the ourRoadMap is undirected, it's enough to perform a dfs from one city only and check if all other cities were visited). ---
--- Note: you can use the function dfs to perform a depth first search in order to explore all reachable cities from the starting city. ---
--- Note: you can use the function elem to check if each city from all cities (from function cities) is present in the visited cities list (from dfs). ---
--- Note: (elem visCities) is shorthand for : (\city -> city elem visCities). ---
--- Note: you can use the function all to check if all the previous elem checks are true. ---    
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected ourRoadMap =
    let allCities = cities ourRoadMap
        visCities = dfs ourRoadMap (head allCities) []
    in  all (`elem` visCities) allCities


--- 8. Implement the function shortestPath :: RoadMap -> City -> City -> [Path] that returns the shortest currentPath between two cities in the roadmap. ---
--- Note: you can use the function dfsShortestPath to get all possible paths between the two cities. ---
--- Note: you can use the function pathDistance to get the distance of each possible currentPath. ---
--- Note: you can use the function minimum to get the minimum distance of all possible paths. ---

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath ourRoadMap startCity endCity = [possibleSP | possibleSP <- allPossiblePaths, pathDistance ourRoadMap possibleSP /= Nothing, pathDistance ourRoadMap possibleSP == minimum (map (\possibleSP -> pathDistance ourRoadMap possibleSP) allPossiblePaths)]
    where allPossiblePaths = dfsShortestPath ourRoadMap startCity endCity []   

dfsShortestPath :: RoadMap -> City -> City -> [City] -> [Path]
dfsShortestPath ourRoadMap currentVisitingCity endCity visitedCities
    | currentVisitingCity == endCity = [[currentVisitingCity]]
    | otherwise = concatMap (\(nextCity, _) ->
        map (currentVisitingCity:) (dfsShortestPath ourRoadMap nextCity endCity (currentVisitingCity:visitedCities))
    ) $ filter (\(c, _) -> c `notElem` visitedCities) (adjacent ourRoadMap currentVisitingCity)

--- Note: Usar biblioteca Bits ---
------------------------------------------------------------------------
--type Distance = Int
--type CitiesDistanceMatrix = Data.Array.Array (Int, Int) (Maybe Distance)
--type AdjacentCities =  [(City,[(City,Distance)])]
--type Bit = Integer
---------------------------------------------------------------------------
--- 9. TravelSales

-- generates adjacent cities in the following manner :
-- [(City,[(City,Distance)])] - Getting us a list of all the cities, with all their adjacent cities and respective distances.

-- using map we apply our function that for each city(given by "cities" (returns a list with all cities))
-- gives us a tuple of the city and their adjacent cities(given by "adjancent"(returns a list of tuples that represent the adjacent cities and their respective distance))
generateAdjacentCities :: RoadMap -> AdjacentCities
generateAdjacentCities ourRoadMap = map (\city -> (city, adjacent ourRoadMap city)) $ cities ourRoadMap

-- generates 2D matrix in the following manner:
-- Data.Array.Array (Int, Int) (Maybe Distance) - giving us an array holding the distance between city pairs
generateCitiesDistanceMatrix :: RoadMap -> CitiesDistanceMatrix
-- Data.Array.array constructs a new array with (1st argument)the specified bounds(set by arrayBounds)
-- (Second Argument) fills the array. In this case we want a pair of cities and their respective distances. 
-- This will fill the array with cities from 0 to the limit set.
-- Calculates the distance using the "distance" functions that calculates the distance between 2 given cities
generateCitiesDistanceMatrix ourRoadMap = Data.Array.array arrayBounds ([((startCity,endCity), distance ourRoadMap (show startCity) (show endCity)) | startCity<-[0..limit],endCity<-[0..limit]])
                        where
                              limit = length (Data.List.sort $ cities ourRoadMap) - 1 -- gives the index of the last city in a sorted list of all cities. Used as an upperbound for the matrix indexes 
                              arrayBounds = ((0,0),(limit,limit)) -- defines the bounds of the 2D array. Meaning the cities will vary from 0 to the limit

-- testBit : Checks if a specific bit is set
-- checks if cityToBeChecked has already been visisted by examining the corresponding bit in visitedMask
isCityVisited :: Bit -> Int -> Bool
isCityVisited visitedMask cityToBeChecked = Data.Bits.testBit visitedMask cityToBeChecked

-- shiftL : shifts bits to the left by a specific number. Creates a bitmask(sequece of bits that represent a binary state) with the number of cities.
-- In this case, all cities are set to 1, creating a bit mask like the following example: 
-- (Example: 3 cities -> 1000 and because we subtract 1 => 111 - representing the 3 cities as all visited
setAllCitiesVisited :: Int -> Bit
setAllCitiesVisited numberOfCities = (Data.Bits.shiftL 1 numberOfCities) - 1

-- setBit : Sets a specific bit to 1 (marks city as visited)
updateVisitedCities :: Bit -> Int -> Bit
updateVisitedCities visitedMask cityToBeVisited = Data.Bits.setBit visitedMask cityToBeVisited

-- List of Arguments:
    -- CitiesDistanceMatrix (2D array that stores distances between pairs of cities)
    -- Bit  (represents a bitmask to update after visiting a city(initialy 1, only current city is visited))
    -- Int  (represents the current position in the function (initialy 0, the starting city we choose))
    -- Bit  (represents the bitmask with all cities visited (111..etc))
    -- Path (represents the current path we have travelled so far, updating throughout the function and eventually becoming the final path)
-- Returns a tuple (Distance, Path), that represents the total distance of the final path chosen.
travelSalesRecursiveDynamicProgramming :: CitiesDistanceMatrix -> Bit -> Int -> Bit -> Path -> (Distance, Path)
travelSalesRecursiveDynamicProgramming citiesDistanceMatrix visitedMask currentPos allCitiesVisited currentPath =
    -- Represents the base case that happens when the bitmask in visitedMask is equal to allCitiesVisited bitmask.
    -- No other cities to visit
    if (visitedMask == allCitiesVisited) 
    then
        -- Data.Array.! is used to find the distance between our current city to the starting city
            -- if it exists, returns distance and adds the current city(last city visited) to our final path and reverses it because we append the cities to the begginning of the list
            -- if not, very high value and empty path to basically nullify this path
        case citiesDistanceMatrix Data.Array.! (currentPos, 0) of
            Just dist -> (dist, reverse (show currentPos : currentPath))
            Nothing   -> (100000000, [])
    else
        -- Data.Array.bounds returns the bounds of the array in our case ((0,0), (limit,limit))
        case Data.Array.bounds citiesDistanceMatrix of
            ((_, _), (maxRow, _)) -> -- get the maxRow in our case(limit)
-- Comprehension list with following conditions:
    -- city <- [0..maxRow] to make sure cities are selected from 0 to limit
    -- not(isCityVisited visitedMask city) checks if the city is already visited
-- if a distance exists between the current city and the next city, we call the function recursively with the following arguments
        -- citiesDistanceMatrix, our matrix remains the same
        -- We update the bitmask, signalling that we visited the current city
        -- We sent the city that was neighbouring our current city
        -- allCitiesVisited, our bitmask with all 1's remains the same
        -- we add the city we just visited to the current path. (Note, our path is being created backwards)
    -- (totalDist, path) -> (distToCity + totalDist, path)
        -- the function returns (totalDist, path) and we add the distance to the city to our current total path distance
-- if a distance doesn't exist, we return a very big distance and an empty list to make it undesirable for selection
-- All of this is wrapped inside minimum, in order to select the path with the least total distance
                minimum [ 
                    case citiesDistanceMatrix Data.Array.! (currentPos, city) of
                        Just distToCity ->
                            case travelSalesRecursiveDynamicProgramming 
                                    citiesDistanceMatrix 
                                    (updateVisitedCities visitedMask city) 
                                    city 
                                    allCitiesVisited 
                                    (show currentPos : currentPath) of
                                (totalDist, path) -> (distToCity + totalDist, path)
                        Nothing -> (100000000, [])
                    | city <- [0..maxRow], not (isCityVisited visitedMask city)
                ]

    

travelSales :: RoadMap -> Path
travelSales ourRoadMap = case isStronglyConnected ourRoadMap of -- Check if roadmap is strongly connected(checks if all the cities are connected to each other)
    False -> [] -- False : return empty list as there is no way of having a path that passes through every city
    -- True : returns the second element of the tuple (Distance, Path) and adds the starting city "0" to the Path in order for it to begin and end in the same city
    True -> snd (travelSalesRecursiveDynamicProgramming citiesDistanceMatrix 1 0 allCitiesVisited []) ++ ["0"]
    where
        allCitiesVisited = setAllCitiesVisited (length $ cities ourRoadMap) -- bitmask all set to 1, representing all visited cities
        citiesDistanceMatrix = generateCitiesDistanceMatrix ourRoadMap -- 2D array representing a pair of cities and their respective distance (City,City) (Maybe Distance)





-- Some ourRoadMaps to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4), ("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected ourRoadMap
gTest3 = [("0","1",4),("2","3",2)]
