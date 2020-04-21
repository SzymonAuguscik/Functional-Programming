reverse::Eq a => [a] -> [a]
reverse list = foldr (\x y -> y ++ [x]) [] list
