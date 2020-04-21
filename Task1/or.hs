or::[Bool] -> Bool
or list = foldl (||) False list
