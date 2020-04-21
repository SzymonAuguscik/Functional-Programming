--data
data BST a = NIL | Node a (BST a) (BST a)
          deriving (Show,Eq)

--insert
insert::Ord a => BST a -> a -> BST a
insert NIL a = Node a (NIL) (NIL)
insert (Node a left right) b 
    | b == a = Node a left right
    | b < a = Node a (insert left b) right
    | b > a = Node a left (insert right b)

--empty
empty::BST a -> Bool
empty NIL = True
empty _ = False

--isBinary
maxInTree:: Ord a => BST a -> a
maxInTree NIL = error "Called on empty tree"
maxInTree (Node a NIL NIL) = a
maxInTree (Node a left NIL) = maximum [a, maxInTree left]
maxInTree (Node a NIL right) = maximum [a, maxInTree right]
maxInTree (Node a left right) = maximum [a, maxInTree left, maxInTree right]

minInTree:: Ord a => BST a -> a
minInTree NIL = error "Called on empty tree"
minInTree (Node a NIL NIL) = a
minInTree (Node a left NIL) = minimum [a, minInTree left]
minInTree (Node a NIL right) = minimum [a, minInTree right]
minInTree (Node a left right) = minimum [a, minInTree left, minInTree right]

isBinary::Ord a => BST a -> Bool
isBinary NIL = error "Called on empty tree"
isBinary (Node a NIL NIL) = True
isBinary (Node a left right) = 
    ((maxInTree left) < a) && ((minInTree right) > a) && isBinary left && isBinary right

--search
search::Eq a => BST a -> a -> Bool
search NIL _ = error "Called on empty tree"
search (Node a NIL NIL) b = (a == b)
search (Node a left NIL) b 
    |a == b = True
    |otherwise = search left b
search (Node a NIL right) b 
    |a == b = True
    |otherwise = search right b
search (Node a left right) b
    |a == b = True
    |otherwise = (search left b) || (search right b)

--isBalanced
height::BST a -> Int
height NIL = 0
height (Node a left right) = 1 + maximum [height left, height right]

isBalanced::BST a -> Bool
isBalanced NIL = True
isBalanced (Node a left right) =
    (abs(height left - height right) <= 1) && (isBalanced left) && (isBalanced right)

--traverse
--VLR
traverseVLR::BST a -> [a]
traverseVLR NIL = []
traverseVLR (Node a left right) = [a] ++ traverseVLR left ++ traverseVLR right
--LVR
traverseLVR::BST a -> [a]
traverseLVR NIL = []
traverseLVR (Node a left right) = traverseLVR left ++ [a] ++ traverseLVR right
--LRV
traverseLRV::BST a -> [a]
traverseLRV NIL = []
traverseLRV (Node a left right) = traverseLRV left ++ traverseLRV right ++ [a]
--VRL
traverseVRL::BST a -> [a]
traverseVRL NIL = []
traverseVRL (Node a left right) = [a] ++ traverseVRL right ++ traverseVRL left
--RVL
traverseRVL::BST a -> [a]
traverseRVL NIL = []
traverseRVL (Node a left right) = traverseRVL right ++ [a] ++ traverseRVL left
--RLV
traverseRLV::BST a -> [a]
traverseRLV NIL = []
traverseRLV (Node a left right) = traverseRLV right ++ traverseRLV left ++ [a]

--toString
toString::Show a => BST a -> String
toString NIL = ""
toString (Node a NIL NIL) = show a
toString (Node a left right) = 
    show a ++ "(" ++ (toString left) ++ "," ++ (toString right) ++ ")"

--leaves
leaves::BST a -> [a]
leaves NIL = []
leaves (Node a NIL NIL) = [a]
leaves (Node a left right) = [] ++ leaves left ++ leaves right

--nnodes
nnodes::BST a -> Int
nnodes NIL = 0
nnodes (Node a left right) = 1 + nnodes left + nnodes right

--nsum
nsum::Num a => BST a -> a
nsum NIL = error "Called on empty tree"
nsum (Node a NIL NIL) = a
nsum (Node a left NIL) = a + nsum left
nsum (Node a NIL right) = a + nsum right
nsum (Node a left right) = a + nsum left + nsum right

--tmap
tmap::(a -> b) -> BST a -> BST b
tmap _ NIL = NIL
tmap f (Node a left right) = Node (f a) (tmap f left) (tmap f right)

--remove
remove::Ord a => a -> BST a -> BST a
remove _ NIL = error "Called on empty tree"
remove a (Node b NIL NIL)
    |a == b = NIL
    |otherwise = (Node b NIL NIL)
remove a (Node b left NIL)
    |a == b = left
    |otherwise = Node b (remove a left) NIL
remove a (Node b NIL right)
    |a == b = right
    |otherwise = Node b NIL (remove a right)
remove a (Node b left right)
    |a == b = Node (minInTree right) left (remove (minInTree right) right)
    |otherwise = Node b (remove a left) (remove a right)

--merge
merge::Num a => BST a -> BST a -> BST a
merge NIL NIL = NIL
merge left NIL = left
merge NIL right = right
merge (Node a left1 right1) (Node b left2 right2) =
    Node (a+b) (merge left1 left2) (merge right1 right2)
