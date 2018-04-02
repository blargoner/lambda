# Lambda

This is a toy implementation of the (untyped) lambda calculus in Haskell.

Examples below are from GHCi.

## Terms

Lambda terms are built up using the value constructors `Var` (for variables), `App` (for application), and `Lam` (for abstraction). For example, the term `(\x->(\y->(x y)))` is represented as follows:

```
> Lam "x" $ Lam "y" $ App (Var "x") (Var "y")
(\x->(\y->(x y)))
```

Terms are shown (with `show`) in the notation seen above.

Equality for terms supports alpha-conversion (renaming of bound variables):

```
> Lam "x" (App (Var "x") (Var "y")) == Lam "z" (App (Var "z") (Var "y"))
True

> Lam "x" (App (Var "x") (Var "y")) == Lam "y" (App (Var "y") (Var "y"))
False
```

## Functions

A number of functions are defined on terms. Below are some examples.

### Variables

Computing the set of variables in a term:

```
> vars $ Lam "x" $ App (Var "x") (Var "y")
fromList ["x","y"]
```

Computing the set of free variables in a term:

```
> free $ Lam "x" $ App (Var "x") (Var "y")
fromList ["y"]
```

Computing a closure of a term:

```
> t = Lam "x" $ App (Var "x") (Var "y")
> t
(\x->(x y))
> closed t
False
> closure t
(\y->(\x->(x y)))
```

### Subterms

Computing the subterms of a term:

```
> subterms $ Lam "x" $ App (App (Var "x") (Var "y")) (Var "z")
fromList [x,y,z,(x y),((x y) z),(\x->((x y) z))]
```

### Substitution

Substituting a term for the free occurrences of a variable in a term:

```
> t = Lam "x" $ App (Lam "y" $ Var "y") (App (Var "y") (Var "y"))
> t
(\x->((\y->y) (y y)))
> sub "y" (App (Var "p") (Var "q")) t
(\x->((\y->y) ((p q) (p q))))
```

The term being substituted must be free for substitution!

```
> sub "y" (App (Var "p") (Var "x")) t
*** Exception: Invalid substitution!
```

### Reduction

Beta-reducing a term (one step, leftmost):

```
> t = Lam "x" $ App (Lam "y" $ Var "y") (Var "x")
> t
(\x->((\y->y) x))
> reduceLeftOnce t
(\x->x)
```

Beta-reduction performs alpha-conversion if necessary.

Normalizing a term:

```
> t = App (App (Lam "x" $ Lam "y" $ App (Var "y") (Var "x")) (Var "a")) (Var "b")
> t
(((\x->(\y->(y x))) a) b)
> normal t
False
> normalize t
(b a)
```

See the module source code for more.

## Combinators

A number of standard combinators are defined.

### I, K, S

```
> i
(\x->x)
> k
(\x->(\y->x))
> s
(\x->(\y->(\z->((x z) (y z)))))
> skk = App (App s k) i
> skk
(((\x->(\y->(\z->((x z) (y z))))) (\x->(\y->x))) (\x->x))
> normalize skk == i
True
```

### Fixed-point combinators

Curry's Y combinator:

```
> y
(\f->((\x->(f (x x))) (\x->(f (x x)))))
> yF = App y (Var "F")
> reduceLeftOnce (reduceLeftOnce yF) == App (Var "F") (reduceLeftOnce yF)
True
```

Turing's theta combinator (if you haven't redefined `t`):

```
> t
((\x->(\f->(f ((x x) f)))) (\x->(\f->(f ((x x) f)))))
> tF = App t (Var "F")
> reduceLeftOnce (reduceLeftOnce tF) == App (Var "F") tF
True
```

For an explanation of these combinators, see [here](https://github.com/blargoner/math-y).

### Omega

```
> omega2
((\x->(x x)) (\x->(x x)))
> reduceLeftOnce omega2 == omega2
True
```

### Booleans

```
> true
(\x->(\y->x))
> false
(\x->(\y->y))
```

### Pairing

```
> t = App (App pair (Var "a")) (Var "b")
> normalize $ App first t
a
> normalize $ App second t
b
```

### Arithmetic

Arithmetic with Church numerals:

```
> num 5
(\f->(\x->(f (f (f (f (f x)))))))
> normalize (App (App add (num 2)) (num 3)) == num 5
True
> normalize (App (App mult (num 2)) (num 3)) == num 6
True
> normalize (App (App expn (num 2)) (num 3)) == num 8
True
> normalize (App predc (num 2)) == num 1
True
```
