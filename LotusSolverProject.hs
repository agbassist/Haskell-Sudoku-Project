import Data.List
import Data.String

--------------------------------------------------------------------------------------------------------
------------------Constants
--------------------------------------------------------------------------------------------------------
--used to group together indexes in the same ring
x0 = [0,1,2,3,4,5,6]
x1 = [7,8,9,10,11,12,13]
x2 = [14,15,16,17,18,19,20]
x3 = [21,22,23,24,25,26,27]
x4 = [28,29,30,31,32,33,34]
x5 = [35,36,37,38,39,40,41]
x6 = [42,43,44,45,46,47,48]

--used to group together indexes in the same arc
y0 = [0,7,15,22,30,37,45]
y1 = [1,8,16,23,31,38,46]
y2 = [2,9,17,24,32,39,47]
y3 = [3,10,18,25,33,40,48]
y4 = [4,11,19,26,34,41,42]
y5 = [5,12,20,27,28,35,43]
y6 = [6,13,14,21,29,36,44]

--used to group together indexes in the same reverse-arc
z0 = [0,13,20,26,33,39,46]
z1 = [1,7,14,27,34,40,47]
z2 = [2,8,15,21,28,41,48]
z3 = [3,9,16,22,29,35,42]
z4 = [4,10,17,23,30,36,43]
z5 = [5,11,18,24,31,37,44]
z6 = [6,12,19,25,32,38,45]

--------------------------------------------------------------------------------------------------------
------------------Functions
--------------------------------------------------------------------------------------------------------

----------------------------------------------------
------------------Conversions
----------------------------------------------------

--Takes an index from the game array and converts it to the ring number of the spot
indexToRing:: Int -> Int
indexToRing x | elem x x0 = 0 | elem x x1 = 1 | elem x x2 = 2 | elem x x3 = 3 | elem x x4 = 4 | elem x x5 = 5 | elem x x6 = 6 |otherwise = 7

--Takes an index and converts it to the Arc number
indexToArc:: Int -> Int
indexToArc y | elem y y0 = 0 | elem y y1 = 1 | elem y y2 = 2 | elem y y3 = 3 | elem y y4 = 4 | elem y y5 = 5 | elem y y6 = 6 | otherwise = 7

--Takes an index and converts it to the corresponding reverse-arc
indexToReverse::Int->Int
indexToReverse z | elem z z0 = 0 | elem z z1 = 1 | elem z z2 = 2 | elem z z3 = 3 | elem z z4 = 4 | elem z z5 = 5 | elem z z6 = 6 | otherwise = 7

-------------------------------------------------------------------------------------------------------------------------------------------------

--Returns the List of the given ring number
whichArrayX:: Int->[Int]
whichArrayX number | number == 0 = x0 | number == 1 = x1 | number == 2 = x2 | number == 3 = x3 | number == 4 = x4 | number == 5 = x5 | number == 6 = x6 | otherwise = []

--Returns the List of the given given arc number
whichArrayY:: Int->[Int]
whichArrayY number | number == 0 = y0 | number == 1 = y1 | number == 2 = y2 | number == 3 = y3 | number == 4 = y4 | number == 5 = y5 | number == 6 = y6 | otherwise = []

--Returns the List of the given reverse-arc number
whichArrayZ:: Int->[Int]
whichArrayZ number | number == 0 = z0 | number == 1 = z1 | number == 2 = z2 | number == 3 = z3 | number == 4 = z4 | number == 5 = z5 | number == 6 = z6 | otherwise = []

----------------------------------------------------
------------------Check Groups of Values
----------------------------------------------------

--returns true if the value is already in the ring or the index
checkRing::Int->Int->[Int]->Bool
checkRing value index puzzle = elem value [puzzle!!x | x<-(whichArrayX (indexToRing index))]

--returns true if the the value is the given arc of the index
checkArc::Int->Int->[Int]->Bool
checkArc value index puzzle = elem value [puzzle!!y | y<-(whichArrayY (indexToArc index))]

--returns true if the value is in the given reverse-arc of the index
checkReverse::Int->Int->[Int]->Bool
checkReverse value index puzzle = elem value [puzzle!!z | z<-(whichArrayZ (indexToReverse index))]

--returns true if the value is valid
checkSpot::Int->Int->[Int]->Bool
checkSpot value index puzzle = not ((checkRing value index puzzle) || (checkArc value index puzzle) || (checkReverse value index puzzle))

----------------------------------------------------
------------------Helper Methods for main solver
----------------------------------------------------

--check if the List is completely filled, returns true if so
solved::[Int]->Bool
solved puzzle = not (elem 0 puzzle)

--find the first empty spot. starting from a given index
findEmptyByIndex::Int->[Int]->Int
findEmptyByIndex startIndex puzzle | solved puzzle = 100 | puzzle!!startIndex == 0 = startIndex | otherwise = findEmptyByIndex (startIndex + 1) puzzle

--find the first empty spot, always starts at the beginning of the list
findEmpty::[Int]->Int
findEmpty puzzle = findEmptyByIndex 0 puzzle

--finds valid values at a given index
findValuesByIndex::Int->[Int]->[Int]
findValuesByIndex index puzzle = [x | x<-[1..7], (checkSpot x index puzzle)] 

--find valid values for the first empty spot
findValuesInFirstEmpty::[Int]->[Int]
findValuesInFirstEmpty puzzle = findValuesByIndex (findEmpty puzzle) puzzle

--edits the value in an list given by index
editValue::Int->Int->[Int]->[Int]
editValue number index puzzle = (take index puzzle) ++ number:[] ++ (drop (index + 1) puzzle)

--edits the first empty spot of the list
editFirstEmpty::Int->[Int]->[Int]
editFirstEmpty number puzzle = editValue number (findEmpty puzzle) puzzle

--find the value of the last spot where the two compared lists have a different value
findLastDifferentValue::[Int]->[Int]->Int
findLastDifferentValue original changed | original == [] = -1 | last original == last changed = findLastDifferentValue (init original) (init changed) | otherwise = last changed

--finds the index of the last spot where the two compared lists have a different value
findIndexLastDifferentValue::[Int]->[Int]->Int
findIndexLastDifferentValue original changed | original == [] = -1 | last original == last changed = findIndexLastDifferentValue (init original) (init changed)| otherwise = (length changed) - 1

----------------------------------------------------
------------------Solving
----------------------------------------------------

--first parameter is values to try, second is the original puzzle (List) , third is the current iteration of the puzzle, returns the solve puzzle
--this method is accessed by lotusSolver, which basically assigns initial values for this method based on one input puzzle
solve::[Int]->[Int]->[Int]->[Int]

--this pattern match "goes back" (kind of like recursing backwards) a step and adjusts the values List parameter so that it only contains values which have not been tried yet in the previous open spot
solve [] original puzzle | values == [] && (findEmpty new) == (findEmpty original) = [] --this guard is the case where the puzzle cannot be solved, returns an empty array
						 | otherwise = solve values original new
						 where new = editValue 0 (findIndexLastDifferentValue original puzzle) puzzle; values = [x|x<-(findValuesInFirstEmpty new), x > findLastDifferentValue original puzzle]

solve (0:[]) original puzzle | solved puzzle = puzzle | otherwise = solve (findValuesInFirstEmpty puzzle) original puzzle --meant to be the default parameters, finds the valid values in the first empty spot, and recurses using that List of values

solve values original puzzle = solve (0:[]) original (editFirstEmpty (values!!0) puzzle) --this is for a List of valid values							 
	

							 
----------------------------------------------------
------------------lotusSolver
----------------------------------------------------

--Only takes in a puzzle(solved or unsolved) as input( returns the solved puzzle. Uses the solve function with standard initial values.
lotusSolver::[Int]->[Int]
lotusSolver puzzle = solve (0:[]) puzzle puzzle --starts with a [0] list to initizalize the solve method

--------------------------------------------------------------------------------------------------------
------------------Testing
--------------------------------------------------------------------------------------------------------

--used as a helper function for checkSolutionValid
checkSolutionValidAtIndex::Int->[Int]->Bool
checkSolutionValidAtIndex i puzzle | not (solved puzzle) = False | i > 48 = True | checkSpot (puzzle!!i) i (editValue 0  i puzzle) = checkSolutionValidAtIndex (i+1) puzzle | otherwise = False

--returns true if the final solution is valid under the constraints of sudoku rules
checkSolutionValid::[Int]->Bool
checkSolutionValid [] = False
checkSolutionValid puzzle = checkSolutionValidAtIndex 0 puzzle 

--returns true if the final List is identical to the original in terms of non-zero terms
identityCheck::[Int]->[Int]->Bool
identityCheck [] final = True
identityCheck original [] | original /= [] = False | otherwise = True
identityCheck original final | head original == 0 = identityCheck (tail original) (tail final) | head original == head final = identityCheck (tail original) (tail final) | otherwise = False

--used to produce output of whether the solver is working correctly
tester::[Int]->String
tester puzzle | final == [] = results ++ "\n\nThe given puzzle has no solution!" ++ "\n\n\n"
			  | otherwise = results ++ "\n\n" ++ "Same identity? " ++ show (identityCheck puzzle final) ++ "\n\n" ++ "Valid Solution? " ++ show (checkSolutionValid final) ++ "\n\n\n"
			  where final = lotusSolver puzzle; results = "\n\n\nOriginal: " ++ show puzzle ++ "\n\n" ++ "Final solution: " ++ show final 

--------------------------------------------------------------------------------------------------------
------------------Main
--------------------------------------------------------------------------------------------------------


main = do		
	   putStr $ tester [5,0,0,0,1,6,0,0,0,0,3,0,0,0,7,0,6,2,1,0,0,0,1,7,0,0,6,0,0,5,0,3,6,7,2,0,0,2,1,0,0,4,0,0,4,0,0,1,0]
	   putStr $ tester [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]
	   putStr $ tester [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0] 
	   putStr $ tester [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]
	   putStr $ tester [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]
	   putStr $ tester [0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0]
	   putStr $ tester [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]
	   putStr $ tester [0,0,3,7,6,1,4,4,0,5,1,7,0,0,0,2,6,0,4,5,3,1,0,0,2,0,0,5,6,0,7,0,0,2,3,2,3,0,7,6,0,0,0,4,5,6,1,7,0] --no solution
		

