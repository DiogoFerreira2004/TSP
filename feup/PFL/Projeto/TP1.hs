import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
    | x <= y = x : (y:ys)
    | otherwise = y : insert x ys

isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

--- 1. Implement the function cities :: RoadMap -> [City] that returns the list of cities in the roadmap, without repetitions. ---
--- Note: you can use the function nub from Data.List to remove repetitions from a list. ---
--- Note: you can use the function concatMap from Data.List to concatenate the lists of cities from the tuples of the roadmap. ---
cities :: RoadMap -> [City]
cities ourRoadMap = isort $ Data.List.nub $ concatMap (\(a,b,_) -> [a,b]) ourRoadMap

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
adjacent ourRoadMap city1 = map (\(c1,c2, d) -> if c1 == city1 then (c2, d) else (c1, d)) $ filter (\(c1, c2, _) -> (city1 == c1) || (city1 == c2)) ourRoadMap

--- 5. Implement the function pathDistance :: RoadMap -> Path -> Distance that returns the total distance of a path in the roadmap. ---
--- Note: you can use the function zip to get the pairs of cities in the path. ---
--- Note: you can use the distance function to get the distance between the city and the adjacent city. ---
--- Note: you can use the function map to get the distances between the pairs of cities. ---
--- Note: you can use the function sum to get the total distance of the path. ---
pathDistance :: RoadMap -> Path -> Distance
pathDistance ourRoadMap ourPath = sum $ map (\(c1, c2) -> case distance ourRoadMap c1 c2 of
    Just d -> d
    Nothing -> 0) $ zip ourPath (tail ourPath)

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]