# hsh
A Unix shell written in Haskell.

### Why?
Well, a few reasons:

1. Because I can.
2. Shells exercise most of the capabilities a production-ready language needs, from I/O to process management to file and directory operations to string parsing to state management. About the only thing shells don't do much of is network I/O.
3. It's accessible. My first Haskell project was an OpenSSH-compatible SFTP server which is _somewhat_ obscure.

## Overview

### Features
* Single-command parser
* Support for env vars through `setenv` and `getenv`

### Misfeatures
* Attempting to run a command that doesn't exist crashes the shell
* Mis-invoking builtins (like `setenv foo` when the correct invocation is `setenv foo bar`) causes the shell to treat `setenv` as an external command.


### Non-Features
This is a programming exercise focused on an interactive command interpreter. I haven't much interest right now in building support for Bourne-style scriptability.

### To-Do List
1. Basic command-line interpreter.
	1. (DONE) Shell-ish parser
	1. (DONE) `ENV` var support
	1. (DONE) Trivial prompt generator
	1. Fundamental builtins
1. Command Look-Up Table creation from `$PATH`
1. `rehash` builtin
1. Smart `rehash` based on `$PATH` changes and `mtime`s of each dir named in `$PATH`
1. Command-name tab completion
1. Argument tab completion
1. Output redirection
1. Programmable prompts