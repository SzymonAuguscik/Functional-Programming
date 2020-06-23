module Student where

data Student = Student {firstName::String, lastName::String, age::Int} 
   deriving (Show, Read, Eq)

data StudentFirstNameChangeEvent = StudentFirstNameChangeEvent {oldName::String, newName::String}
   deriving (Show, Read, Eq)
listToProcess = [Student "Alicja" "Akla" 21, Student "Batrek" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "Damian" "Dab"  22, Student "Eustachy" "Elo" 20]
modifiedList = [Student "AlicjaX" "Akla" 21, Student "BatrekX" "Bodo" 20, Student "Celina" "Czyzyk" 21, Student "DamianX" "Dab"  22, Student "Eustachy" "Elo" 20]

--fullName
toFullName::Student -> String
toFullName (Student fn ln _) = fn++" "++ln

fullNamesList::[Student] -> [String]
fullNamesList studentsList = map toFullName studentsList

loadMembers::String -> IO()
loadMembers members = do
			contents <- readFile members
			let
				studentsList = map (\x -> (Student (x!!0) (x!!1) (read(x!!2)::Int))) (map words (lines contents))
			mapM_ (\x -> putStrLn (toFullName x)) studentsList

--tuple
tupleList::[Student] -> [(Int, Student)]
tupleList studentsList = zip [1..] studentsList

--txtReport
toTxt::(Int, Student) -> String
toTxt (n, (Student fn ln a)) = (show n)++". student: "++ln++" "++[head fn]++". wiek: "++(show a)

createReport::[Student] -> String
createReport studentsList = foldl(\x y -> x++y++"\n") "" (map toTxt (tupleList studentsList))

showReport::[Student] -> IO()
showReport studentsList = putStr (createReport studentsList)

saveReport::String -> [Student] -> IO()
saveReport filePath studentsList = writeFile filePath (createReport studentsList)

--HTML
toTD::String -> String
toTD sth = "<td>"++sth++"</td>"

toTR::String -> String
toTR sth = "<tr>\n"++sth++"\n</tr>"

toTH::String -> String
toTH sth = "<th>"++sth++"</th>"

mapToTD::Student -> String
mapToTD (Student fn ln a) = (toTD fn)++"\n"++(toTD ln)++"\n"++(toTD (show a))

createHTML::[Student] -> String
createHTML list = "<!DOCTYPE hmtl>\n<html>\n<body>\n<table>\n"++
			foldl(\x y -> x++y++"\n") "" (map toTH headList)++
			foldl(\x y -> x++y++"\n") "" (map toTR tmpList)++
			"</table>\n</body>\n</html>\n"
		        where 	tmpList = (map mapToTD list)
				headList = ["FirstName", "LastName", "Age"]

showHTML::[Student] -> IO()
showHTML studentsList = putStr (createHTML studentsList)

saveHTML::String -> [Student] -> IO()
saveHTML filePath studentsList = writeFile filePath (createHTML studentsList)

--event
findChanges::[Student] -> [Student] -> [StudentFirstNameChangeEvent]
findChanges [] [] = []
findChanges (x:xs) (y:ys)  | (firstName x) == (firstName y) = [] ++ findChanges xs ys
			   | otherwise = [StudentFirstNameChangeEvent (firstName x) (firstName y)] ++ findChanges xs ys

showChanges::[Student] -> [Student] -> IO()
showChanges list1 list2 = mapM_ (\x -> putStrLn ("Changed from "++(oldName x)++" to "++(newName x))) list where list = findChanges list1 list2
