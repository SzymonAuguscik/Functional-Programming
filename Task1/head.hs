head::Eq a => [a] -> a
head list = foldr (\h t -> h) undefined list
