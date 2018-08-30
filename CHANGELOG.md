# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).  


## 0.2.0 
### Added 
- Commented code for all! Every function has comments, so if you want to contribute, contribute away! The comments give a general description of the uses of the function as well
- Much better error descriptions in the REPL, clarifies anything that isn't already simple enough
- Vectors -- they are commented out because the syntax is really weird, but if you want to go through and try to find out how they work, by all means go ahead -- Vectors are decalred around the lines of `#( values )` 
- Legacy folder with the old "Parser.hs" file 
- Added comments to BuBBLE code --  `;` is a single line comment and multiline comments start with `:=` and ends with `=:` 
- `exp` function that works like exponential numbers -- ex: 2^2 = `(exp 2 2)`

### Changed
- Split up "Parser.hs" into "Types.hs", "Primitives.hs" and "Parser.hs" because this is a Haskell project, not Shell! Also, it was split also to increase readability
- odd? and even? is now part of the language instead of being in stdlib.ble
- `lambda` function is now called `use` 
- the Unix executable `init`, which installs the REPL on the PATH, now says that the repository was installed in the HOME directory *if* the repository/zip file was moved to a different location (it makes more sense when you use it)



## 0.1.1
### Added 
- gcd: function for finding the greatest common denominator
- better printing: strings no longer contain double quotes ( "" ) when printed in the REPL 
- inc/dec: functions to INCrease or DECrease a number by one (contained in stdlib)

### Changed
- lt & gt: Their functions were accidentally flipped

### Bug Fixes
- Ratio/Complex/Float: These data types are now properly parsed in the REPL

### Extra Notes
- Still working on bugs, WIP
- .zip on the release page



## 0.1.0 
### Added
- BuBBLE repository 
