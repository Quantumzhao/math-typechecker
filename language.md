## Relation

### Definition

To define a relation $R$ such that $A\ R\ B$, use the following syntax

```
R := [tags] Relation between A and B (where [clause])
```

This is exactly the same as the mathematical equivalent, for set $A$ and $B$, $R$ is the cartesian product $A'\times B'$ where $A'\subseteq A$ and $B'\subseteq B$. 

In other words, $R$ holds for some $a\in A$, and some $b\in B$, but there is no claim of which elements are related by $R$

### Evidence

We can supply claims that a single $a\in A$ and $b\in B$ are related, i.e. $a\sim_R b$. The syntax is

```
let a ~ b by R
```

Or if we want to claim some $a$ and $b$ are related, given that $\forall a\in A'$, $\forall b\in B'$, $a\sim_R b$ or alternatively $A'\times B'\subseteq A\times B$: 

```
let A' ~ B' by R
```

After supplying this evidence, the program will be assured to use these relations in type inference. 

In contrast, if no evidence is supplied, even if the relation is defined, the program cannot be sure if any specified element is indeed inside the relation. 

> Note: an evidence is not a definition, so the program will still throw error even if single instance of evidence is claimed. 

## Mapping

```
f := [tags] Mapping A -> B (where [clause])
```

## Object

```
e := element of A (where [clause])
```

## Set

```
A := [tags] Set (where [clause])
```

## Tuple

yet to be determined
