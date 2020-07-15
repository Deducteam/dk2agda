{-# OPTIONS -W noMissingDefinitions #-}
module sttfa where
type : Set
arrow : (type -> (type -> type))
bool : type
eta : (type -> Set)
ptype : Set
p : (type -> ptype)
etap : (ptype -> Set)
forallK : ((type -> ptype) -> ptype)
eps : ((eta (bool)) -> Set)
impl : ((eta (bool)) -> ((eta (bool)) -> (eta (bool))))
dk^forall : (t : type) -> (((eta (t)) -> (eta (bool))) -> (eta (bool)))
forallP : ((type -> (eta (bool))) -> (eta (bool)))
eta  = \(t : type) -> (etap ((p (t))))
etap (p (((arrow (0::l)) (1::r)))) = ((eta (0::l)) -> (eta (1::r)))
etap (forallK (0::f)) = (x : type) -> (etap ((0::f (x))))
eps ((dk^forall (0::t)) (1::f)) = (x : (eta (0::t))) -> (eps ((1::f (x))))
eps ((impl (0::l)) (1::r)) = ((eps (0::l)) -> (eps (1::r)))
eps (forallP (0::f)) = (x : type) -> (eps ((0::f (x))))
