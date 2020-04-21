tail::Eq a => [a] -> a
tail list = foldl (\h t -> t) undefined list
