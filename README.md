# write-yourself-a-scheme

Back in mid Oct 2015, I started a side project that after a few commits in GitHub was going to be abandoned until about a month ago (Jan 2021), when I decided to retake it.

This is how you build the code from its source: `stack build`

Here’s what you can do with it:
* Run it `stack exec write-yourself-a-scheme-exe`
* Load a file in the REPL `(load "src/my-test.scm")`
* Execute stuff in it `test1`, `test2`, etc.

The primitives that had to be baked in to the implementation in order to allow a standard lib,  are:
* Arithmetic operators (+, -, *, /, etc)
* Boolean operators (>, <, &&, ||)
* List operators (car, cdr, cons)

It’s far from a complete implementation, there are missing language features (`let`, comments, etc).

The idea that you can build an AST to represent a language,
* Can be transferred to model and implement core domain business logic too.
* With their evaluation being slightly different.

The initial starting point was this wiki book [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) by Jonathan Tang.

What this repo adds is:
* A build system
* Dependency management
* Tests as documentation
* And, an attempt to a more modular project structure
