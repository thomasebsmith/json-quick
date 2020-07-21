# json-quick
**A fast command-line utility for dealing with JSON**

[Github](https://github.com/thomasebsmith/json-quick)

`json-quick` provides one fast, easy-to-use command for dealing with JSON
on the command line.

## Installation
Currently, `json-quick` must be cloned via Git and compiled from source.
See the [project README](https://github.com/thomasebsmith/json-quick/#readme)
for more detailed instructions.

## Usage
`json-quick`'s features are separated into a number of commands. Currently,
the following commands are available:
 - [`help`](#help-command) - Displays usage and version information.
 - [`prettify`](#prettify-command) - Pretty-prints any JSON given as input.
 - [`select`](#select-command) - Selects a subset of JSON given as input.
 - [`verify`](#verify-command) - Verifies that and JSON given as input is
   specification-conformant.

## General Options
These options are available for any command:
 - `--in <file>`, `-i <file>`: Reads input from *<file>* instead of stdin.
 - `--out <file>`, `-o <file>`: Writes output to *<file>* instead of stdout.

## Help Command
Displays general and command-specific usage and version information.

Options:
 - `--with <command>`, `-w <command>`: Displays usage information for
   *<command>*.

## Prettify Command
Pretty-prints any JSON given as input. Currently, this command always indents
with two spaces. However, options to customize indentation will be available
in the future.

## Select Command
Selects a subset of the JSON that is given as input. Uses
[selection syntax](#selection-syntax) for patterns.

Options:
 - `--pattern <pattern>`, `-p <pattern>`: Selects using *<pattern>*. If no
   pattern is specified, the entire JSON object is selected.

## Verify Command
Ensures that its input is valid JSON (conformant to the official specification).
Returns with a zero exit code and prints "Valid JSON" to output if the JSON
is valid.
Returns with a non-zero exit code and prints an error to stderr if the JSON
is not valid.

## Selection Syntax
To select part of a JSON object, you can use dot-separated parts. Each part
can be a double-quoted property name, a non-quoted property name, or an
asterisk, which matches all property names.

For example, the syntax `my_property."a.key".0"` would select an object's
`my_property` property, then the `a.key` property of that object, and finally
the first element of that array.
