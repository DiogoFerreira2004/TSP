import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


--- 1. Implement the function cities :: RoadMap -> [City] that returns the list of cities in the roadmap, without repetitions. ---
--- Note: you can use the function nub from Data.List to remove repetitions from a list. ---
--- Note: you can use the function concatMap from Data.List to concatenate the lists of cities from the tuples of the roadmap. ---
cities :: RoadMap -> [City]
cities ourRoadMap = Data.List.nub $ concatMap (\(a,b,_) -> [a,b]) ourRoadMap

--- 2. Implement the function areAdjacent :: RoadMap -> City -> City -> Bool that returns True if the two cities are adjacent in the roadmap, and False otherwise. ---
--- Note: you can use the function any from to check if there is any tuple in the roadmap that has the two cities. ---
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent ourRoadMap city1 city2 = any (\(c1, c2, _) -> (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)) ourRoadMap 

--- 3. Implement the function distance :: RoadMap -> City -> City -> Maybe Distance that returns the distance between two cities in the roadmap, if they are adjacent, and Nothing otherwise. ---
--- Note: you can use the function filter to get the tuples that have the two cities. ---
--- Note: you can use pattern matching to get the distance from the tuple. ---
distance :: RoadMap -> City -> City -> Maybe Distance
distance ourRoadMap city1 city2 = case filter (\(c1, c2, _) -> (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)) ourRoadMap of
    [] -> Nothing
    [(_, _, d)] -> Just d

--- 4. Implement the function adjacent :: RoadMap -> City -> [(City,Distance)] that returns the list of cities adjacent to a given city, with the respective distances. ---
--- Note: you can use the function map to transform the tuples of the roadmap into tuples with the city and the distance. ---
--- Note: you can use the function filter to get the tuples that have the city. ---
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent ourRoadMap city1 = (map (\(c1,c2, d) -> if c1 == city1 then (c2, d) else (c1, d)) . filter (\(c1, c2, _) -> (city1 == c1) || (city1 == c2))) ourRoadMap

--- 5. Implement the function pathDistance :: RoadMap -> Path -> Distance that returns the total distance of a path in the roadmap. ---
--- Note: you can use the function zip to get the pairs of cities in the path. ---
--- Note: you can use the distance function to get the distance between the city and the adjacent city. ---
--- Note: you can use the function map to get the distances between the pairs of cities. ---
--- Note: you can use the function sum to get the total distance of the path. ---
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
    let cityDegrees = map (\city -> (city, length $ filter (\(c1, c2, _) -> city == c1 || city == c2) ourRoadMap)) $ cities ourRoadMap 
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
--- Note: you can use the function head to get the first city of the list (Since the graph is undirected, it's enough to perform a dfs from one city only and check if all other cities were visited). ---
--- Note: you can use the function dfs to perform a depth first search in order to explore all reachable cities from the starting city. ---
--- Note: you can use the function elem to check if each city from all cities (from function cities) is present in the visited cities list (from dfs). ---
--- Note: (`elem` visCities) is shorthand for : (\city -> city `elem` visCities). ---
--- Note: you can use the function all to check if all the previous elem checks are true. ---    
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected ourRoadMap =
    let allCities = cities ourRoadMap
        visCities = dfs ourRoadMap (head $ allCities) []
    in  all (`elem` visCities) allCities


--- 8. Implement the function shortestPath :: RoadMap -> City -> City -> [Path] that returns the shortest path between two cities in the roadmap. ---
--- Note: you can use the function dfsShortestPath to get all possible paths between the two cities. ---
--- Note: you can use the function pathDistance to get the distance of each possible path. ---
--- Note: you can use the function minimum to get the minimum distance of all possible paths. ---

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath ourRoadMap startCity endCity = [possibleSP | possibleSP <- allPossiblePaths, pathDistance ourRoadMap possibleSP /= Nothing, pathDistance ourRoadMap possibleSP == minimum (map (\possibleSP -> pathDistance ourRoadMap possibleSP) allPossiblePaths)]
    where allPossiblePaths = dfsShortestPath ourRoadMap startCity endCity []   

dfsShortestPath :: RoadMap -> City -> City -> [City] -> [Path]
dfsShortestPath ourRoadMap currentVisitingCity endCity visitedCities
    | currentVisitingCity == endCity = [[currentVisitingCity]]
    | otherwise = concatMap (\(nextCity, _) -> map (currentVisitingCity:) (dfsShortestPath ourRoadMap nextCity endCity (currentVisitingCity:visitedCities))) $ filter (\(c, _) -> c `notElem` visitedCities) (adjacent ourRoadMap currentVisitingCity)

--- Note: Usar biblioteca Bits ---

transformRoadMap :: RoadMap -> [(City, [(City, Distance)])]
transformRoadMap ourRoadMap = map (\city -> (city, adjacent ourRoadMap city)) $ cities ourRoadMap

generateAllCompletePaths :: RoadMap -> [Path]
generateAllCompletePaths ourRoadMap = map (\city -> dfsAllCities ourRoadMap city []) $ cities ourRoadMap 

dfsAllCities :: RoadMap -> City -> [City] -> Path
dfsAllCities currentVisitingCity visitedCities
    | length visitedCities == length (cities ourRoadMap) = [currentVisitingCity]
    | otherwise = concatMap (\(nextCity, _) -> map (currentVisitingCity:) (dfsAllCities ourRoadMap nextCity (currentVisitingCity:visitedCities))) $ filter (\(c, _) -> c `notElem` visitedCities) (adjacent ourRoadMap currentVisitingCity)

travelSales :: RoadMap -> Path
travelSales = undefined


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4), ("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
