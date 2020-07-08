// Generated example file: unary natural numbers.
constant symbol N : TYPE

constant symbol z : N
constant symbol s : N → N

// Enabling built-in natural number literal, and example.

set builtin "0"  ≔ z
set builtin "+1" ≔ s

definition forty_two ≔ 42

// Addition function.

symbol add : N → N → N
set infix left 6 "+" ≔ add

rule z      + $n     ↪ $n
with (s $m) + $n     ↪ s ($m + $n)
with $m     + z      ↪ $m
with $m     + (s $n) ↪ s ($m + $n)
