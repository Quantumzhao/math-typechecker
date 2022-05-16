# Math Type Checker

## Build

`cd` to project directory, and then

```
stack build
```

>  If there is any error related to the version of GHC, try the following:
>
> 1. open `stack.yaml`
> 2. change `resolver: [something]` to `resolver: lts-18.6`
> 3. and try again 

## Run

Inside the project directory, run

```
stack run
```

If everything goes well, there should be a message

> Successfully loaded all definitions

An then you can interact with the CLI. A more detailed documentation on the syntax and supported constructs is in the file `language.md`

### Some Other options

#### Configuration and Dependency

There is one particular file that is worth noticing: `./app/deps.cfg`. It is the list of definition files that will be loaded into the environment upon program initialization. 

By default, the program loads in these files in order:

- `./definitions/common.mathdef`
- `./examples/numbers.mathdef`

> Moving or renaming this file can result in unexpected behavior. (Also it might not work as expected on Windows)

Here is the specification:

1. The paths in the file will be scanned sequentially from top to bottom, which means the first line will be loaded first. Wrong initialization order can result in dependency errors. 
2. No newlines between the entries
3. You can add or remove paths as you wish, as long as they are valid files. 

#### User Defined files

By convention, `common.mathdef` is located in `./definitions`. You are free to move this file around, but it is not recommended to do so. 

You can add your own definition files to 

- `./definitions` if it is part of the definitions of common mathematical structures

- `./examples` if it is something you want to try out but it’s too tedious to type in in the CLI

For example, if you want to test `test.mathdef`, just append its path after the existing lines in `deps.cfg`, and the file will look like this:

```
./definitions/common.mathdef
./examples/numbers.mathdef
./examples/test.mathdef
```

## Examples

> For this section, everything after `>>>` is the user input and the following line is the program output

1. ```haskell
   >>> Natural
   Natural is countable Set Natural
   ```

2. ```haskell
   >>> 1 := element in Natural
   1 is element in Natural
   ```

3. ```haskell
   >>> add1(1)
   natural is element in Natural
   ```

   > How to understand this?
   >
   > If the input is an expression, the program output is always the value that it returns. In this case, the returned value is an object in $\N$ which has not been defined by the user yet. 
   >
   > So the program assigned it with the name *natural*

4. ```haskell
   >>> add1
   add1 is Natural -> Natural
   ```

5. ```haskell
   >>> add1(Natural)
   Natural is countable Set Natural
   ```

6. ```
   >>> 1.1 := element in Real
   1.1 is element in Real
   ```

7. ```haskell
   >>> addR(1.1, 1.1)
   real is element in Real
   ```

8. ```haskell
   >>> add1(1.1)
   applyArg: arg is not related to domain
   ```

   > How to understand this?
   >
   > This is an error message (although not necessarily the most informative) meaning that $1.1\in\R$ does not imply $1.1\in\N$ because there is no claim of relation, such that $\R\subseteq \N$ (and in fact as we all know, $\R\nsubseteq\N$)

9. ```
   >>> A := Set
   ...
   >>> B := Set
   ...
   >>> f := Mapping A -> B
   ...
   >>> f(A)
   B is Set B
   >>> f(B)
   applyArg: arg is not related to domain
   ```

   Normal stuff so far. What if we specify $B\subseteq A$?

   ```haskell
   >>> claim: B isSubsetOf A
   Done.
   >>> f(B)
   B is finite Set B
   ```

   It can now recognize this expression. 

   > Note: `isSubsetOf` is a predefined relation. 
   >
   > If you want to define your own relation, refer to this example: 
   >
   > ```
   > >>> multiple := Relation between Natural and Natural
   > ```
   >
   > However, it is pretty much useless right now because I haven’t got enough time to fully implement other related stuff (poset/lattice/equivalence relation/…)

10. ```haskell
    >>> claim: addR isIn Functions
    >>> compose(addR, addR)
    functions is element in Functions
    ```

    > Again, not a great name, but it works

11. ```haskell
    >>> 1 := element in Natural
    ...
    >>> 4 := element in Natural
    ...
    >>> claim: 1 ~ 4 by multiple
    Done.
    ```

    > You can also use the tilde notation (which is more formal, but less readable)

## Other

I wish my project could be hosted on class homepage
