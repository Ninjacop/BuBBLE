# BuBBLE

![Logo](https://github.com/Ninjacop/BuBBLE/blob/master/Bubble.png)

## Installation - Tested on Intel Mac, but I Think It Runs on Certain Linux OSes 
    
### Downloading via Git
In terminal, type 
    
    $ git clone https://github.com/Ninjacop/BuBBLE


### Download via .zip
Download the .zip file from this repository and make a folder in your home directory called "BuBBLE" and put all the files in there.

### After Downloading 
Navigate to your home directory and go to "BuBBLE/bin"

Next, double click/drag "init" into terminal and press enter. After that, it should be installed on your PATH and ready to go!

Type "bubble repl" to see if it worked



## Usage
    
On the command line (as of 0.1.0) you can do two things

    $ bubble repl
which opens a REPL

and 

    $ bubble stand x
which downloads library "x" from [bubblestand], and installs it into BuBBLE/bin
    

      
## Examples  
      
See the [wiki](https://github.com/Ninjacop/BuBBLE/wiki) for examples/tutorials
      
## Bugs  
      
Unquote can appear outside of a quasiquoted list

Unquote Splicing csn appear outside of a quasiquoted list  
      
 
## TODO/Rambling
So my goal for BuBBLE is to use one or more Hackages to transform the functions in Haskell into usable functions in BuBBLE

Right now, a lot of functions return `IO ()` or some variation of `IO Type`. The thing is that BuBBLE data type goes under this process:

Define a type -> use for whatever purpose -> print to REPL by showVal


### The Problem
Since a lot of functions in external Hackages return `IO ()` and showVal freaks out and throws `Couldn't match type ‘IO ()’ with ‘[Char]’`, which `[Char]` is just a String, I/other people who are willing to help need to figure out how to show `IO ()` in the REPL.

Also, I found out that LITERALLY NOTHING can be done with `IO Type` in showVal, so that's a problem. 

The only thing that works with `IO` in showVal is doing this:

    showVal (IO Type x) = "example"

But the problem with that is when evaluated in the REPL, it doesn't do anything, it just prints out `"example"`.

Definition of showVal:

    showVal :: Values -> String

## License  
      
Copyright © 2018 Ninjacop123

See LICENSE for an in-depth look 
