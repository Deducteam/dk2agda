# dk2agda

A tool to convert files written in Dedukti to Agda

## Dependencies

It depends heavily on lambdapi (hence all of its dependencies are required).

## Build

To build the binaries, use the following :

```sh
make install
```

## Scripts

### dk2agda.ml

Main export script. It takes a dk/lp file and an output directory as arguments and returns the new agda file in the output directory.
Note that with this script, the output dir must exist beforehand.

### dk2agda.sh

Utility script that will, later, handle the arguments, files, directories, etc, for dk2agda.ml.
It is a more intuitive way to use dk2agda since the output directory will be created if it doesn't exist and one can pass multiple files located in a single directory at once.

## Issues

### Solvable

We need to have lhs\_typing from lambdapi/src/core/sr.ml to build the lhs since after subject reduction, the meta variable that where instantiated with a value are replaced with fresh variables.  

To do this, I changed lambdapi's code to have it available in the rules but I need to find a way to obtain it without changing lambdapi.  

Modifications of lambdapi goes as follow :

```OCaml
(*terms.ml  153 :*) (** Number of variables in RHS but not in LHS. *) 
                    ; lhs_typing : term
                    (** Left hand side after subject reduction *) }
(*handle.ml 356 :*) ; xvars_nb = pur.pr_xvars_nb 
                    ; lhs_typing = Type (* placeholder *)}
(*sr.ml     148 :*) let naive_rule = {lhs; rhs; arity; arities; vars; xvars_nb = 0; lhs_typing = Type} in
(*sr.ml     264 :*) { lhs ; rhs ; arity ; arities ; vars; xvars_nb = 0 ; lhs_typing = lhs_typing }
```

### Unsolvable

A lhs in Dedukti/Lambdapi doesn't have to be well-typing. In Agda this is not true and for example the following deduckti code is impossible to export :  

```dedukti
def A : Type.
a : A.
B : Type.

def eA : A -> Type.
[] A --> eA a.

def eB : B -> Type.
[] eB a --> eA a.
```

In Agda, we will have `(eA a) !=< B when checking the expression a has type B`  

Also, Agda doesn't seem to support lambda abstraction in the lhs. Files that use this features can't be exported as well.  
