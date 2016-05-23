# hsh
A Unix shell written in Haskell.

### Why?
Well, a few reasons:

1. Because I can.
2. Shells exercise most of the capabilities a production-ready language needs, from I/O to process management to file and directory operations to string parsing to state management. About the only thing shells don't do much of is network I/O.
3. It's accessible. My first Haskell project was an OpenSSH-compatible SFTP server which is _somewhat_ obscure.

## Overview

### Features
Currently: None

### Non-Features
This is a programming exercise focused on an interactive command interpreter. I haven't much interest right now in building support for Bourne-style scriptability.

### To-Do
1. Basic command-line interpreter.
	1. Shell-ish parser
	1. `ENV` var support
	1. Trivial prompt generator
	1. Fundamental builtins
1. Command Look-Up Table creation from `$PATH`
1. `rehash` builtin
1. Smart `rehash` based on `$PATH` changes and `mtime`s of each dir named in `$PATH`
1. Command-name tab completion
1. Argument tab completion
1. Output redirection
1. Programmable prompts