# TODO
So my goal for BuBBLE is to use one or more Hackages to transform the functions in Haskell into usable functions in BuBBLE, which creates a "purpose" for this language/DSL

Right now, a lot of functions return `IO ()` or some variation of `IO Type`. The thing is that BuBBLE data type goes under this process:

Define a type -> use for whatever purpose -> print to REPL by showVal


## The Main Problem 
Since a lot of functions in external Hackages return `IO ()` and showVal freaks out and throws `Couldn't match type ‘IO ()’ with ‘[Char]’`, which `[Char]` is just a String, I/other people who are willing to help need to figure out how to show `IO ()` in the REPL.

Also, I found out that LITERALLY NOTHING can be done with `IO Type` in showVal, so that's a problem. 

The only thing that works with `IO` in showVal is doing this:

    showVal (IO Type x) = "example"

But the problem with that is when evaluated in the REPL, it doesn't do anything, it just prints out `"example"`.

Definition of showVal:

    showVal :: Values -> String


## Other Things that I'm Currently Trying to Work Out

random numbers -- ex: (random 1 10) -> 5

do blocks -- like `progn` or `do`

while loops and more types of iteration -- like `dotimes` and `loop` 
