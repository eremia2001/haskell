data NAT = Null | Succ NAT deriving Show 
plus :: NAT -> NAT -> NAT
mal :: NAT -> NAT -> NAT
plus n Null = n 
plus n (Succ m) = Succ (plus n m)
mal n Null = Null 
mal n (Succ m) = plus (mal n m) n
