# Syntax

## Relation

### Definition

To define a relation $R$ such that $A\ R\ B$, use the following syntax

```
R := [tags] Relation between A and B (where [clause])
```

This is exactly the same as the mathematical equivalent, for set $A$ and $B$, $R$ is the cartesian product $A'\times B'$ where $A'\subseteq A$ and $B'\subseteq B$. 

In other words, $R$ holds for some $a\in A$, and some $b\in B$, but there is no claim of which elements are related by $R$

### Evidence/Claim

We can supply claims that a single $a\in A$ and $b\in B$ are related, i.e. $a\sim_R b$. The syntax is

```
a ~ b by R
```

Or the infix equivalent

```
a R b
```

---

In contrast, if no evidence is supplied, even if the relation is defined, the program cannot be sure if any specified element is indeed inside the relation. 

> Note: a claim is not a definition, so the program sometimes may still throw error even if some claims (on other elements) are stated. 

## Mapping

```
f := [tags] Mapping A -> B
```

If the mapping is a binary operation:

```
f := [tags] Mapping (A, B) -> C
```

## Object

```
e := element in A
```

## Set

```
A := [tags] Set
```

> Note there is no way of explicitly defining a class, because class doesn’t have many good properties like sets

## Tuple

```
(a, b)
```

# Expression

## Function application

```
f(a)
```

Or

```
f(a, b)
```

## Variable

Just type in the variable name

# File

Each file can contain multiple entries of definitions. 

Technically, you can write expressions in files as well, but there is no point of doing it because:

1. The output is always omitted
2. The return value of expressions will not be captured by the environment

Also having an `exit` command inside a file is generally considered unconstructive as it will lead to premature demise.  
