{-# OPTIONS -W noMissingDefinitions #-}
module sttfa where

type : Set

arrow : type -> type -> type

bool : type

eta : type -> Set

ptype : Set

p : type -> ptype

etap : ptype -> Set

forallK : (type -> ptype) -> ptype

eps : eta bool -> Set

impl : eta bool -> eta bool -> eta bool

forallF : (t : type) -> (eta t -> eta bool) -> eta bool

forallP : (type -> eta bool) -> eta bool

eta = \ t -> etap (p t)

etap (p (arrow l r)) = eta l -> eta r

etap (forallK f) = (x : type) -> etap (f x)

eps (forallF t f) = (x : eta t) -> eps (f x)

eps (impl l r) = eps l -> eps r

eps (forallP f) = (x : type) -> eps (f x)
