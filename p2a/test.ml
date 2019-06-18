eq (product (insert 2 []) (insert 2 [])) (insert (2,2) []) = true
eq (product  [2] [2] ) [(2,2)] = true

eq (insert (2,3) (insert (2,9) [])) (product (insert 2 []) (insert 3 (insert 9 []))) = true
eq [(2,3);(2,9)](product [2] [3,9]) = true



