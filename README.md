# CMSC488B Project Proposal

## 1. Overall goal

This program will visualize the user defined objects such as sets, groups and their derivatives, as well as providing quick summaries similar to type annotations in Haskell or OCaml. Users can provide such custom definitions via a file written in a DSL or interact with the program CLI. The “types” will be similar to dependent types, but with more subtle features.  

## 2. Use case

When I was studying MATH402 which is Algebraic Structures, I was very much confused by the various concepts and constructs that are densely intertwined together. I wished that I could have some sort of programs/calculators to help me figure out the “type” of objects (i.e. group of elements, group of subgroup and cosets, group of symmetry …). This interactive program could possibly offer a visualization or “type” annotation. 

A crude example: 

Type in:

```
>>> G := Group Z4 +
```

> $G$ is the group of integer mod 4 over addition. 

The output is:

```
G: abelian Group a + where 
	a: [n]_(mod 4) where
		n in [0, 3]
```

> `a: [0]_(mod 4) ... [3]_(mod 4)` means $a$ represents (I don’t know the correct word choice for it. Type? Class? Kind? Category?) the equivalence classes $[0]_\text{mod 4}$, $[1]_\text{mod 4}$, $[2]_\text{mod 4}$ and $[3]_\text{mod 4}$. Because $+$ is a pre-defined associative and commutative operator, it is inferred that this group is abelian. 

Then:

```
>>> f := V -> G where -> is bijective
```

> $f$ is the bijective map from Klein-4 group to $G$

The output is:

```
f: V -> abelian Group a + where 
	a: [n]_(mod 4) where
		n in [0, 3]
    -> is bijective
```

> The program won’t throw any error even if there does not exist such a mapping. It only checks for “type” and assumes the user inputs are correct. 

Then:

```
>>> f^(-1)(G)
```

> The program will also generate a definition of the inverse  for bijective relations, due to the nature of bijectivity. 

The output is:

```
V
```

> $f^{-1}$ can also work on elements of a group

## 3. Project architecture

This program can be roughly divided into the following components:

1. **Lexer**: The program will need to come with a DSL. 
2. **Parser**: Convert the tokens to an AST. 
3. **Evaluator**: Infer the “type” of the given expression. This might require monad transformers. 
4. **Standard library**: Textbook definitions will be selectively chosen and stored in this module. 
5. **“Type” inference algorithm**: Because these mathematical notations are quite different from the typical types in type theory, this part would not be a trivial task. 
6. **“Type” system**: Same as above, but these are the definitions for the data structures holding the “types”. 

## 4. Testing

This project is relatively easy to test. The `quickcheck` properties can solely consist of definitions from a textbook. 

The generators on the other hand, would be tricky to implement. The relations between algebraic structures should not be generated randomly as they usually embody implications. In order to automatically verify and filter out invalid relations, I would certainly need a auto theorem proving algorithm which is impractical to design given the time constraint. Thus I would probably manually populate the candidate/example pool. 

For example, 

1. One simple property: 

   > If $A$ and $B$ are sets, then their cartesian product can be denoted by $A\times B$

   `quickcheck` should be able to read results from standard output and then compare the literal strings to the textbook definition in the program DSL’s notation. 

2. Checking the types of user defined objects

   This should be straight forward as well, as I am able to give expected “types” by myself and then allow the `quickcheck` to compare with the actual result. The “types” are either equal or not equal, which makes it extremely easy to assert. 