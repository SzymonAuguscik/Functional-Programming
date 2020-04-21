and::[Bool] -> Bool
and list = foldl (&&) True list
