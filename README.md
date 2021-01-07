# Nolfaris

## Description

Nolfaris is a command line utility which can be used in order to compile the NoKe programming language.

NoKe is a strictly typed and programming language designed to be used with the Arcadia Engine as a scripting language. The language is also designed to facilitate its IDE integrations, and an official Visual Studio Code will be made to support syntax highlighting, autocompletion, linting, snippets, debugging etc.

The compiler is written in Rust and uses Pest as a parser generator.

## Roadmap

Here is a roadmap of the planned features:

- [x] Parser
  - [x] Expressions
    - [x] Literals
      - [x] Decimal int
      - [x] Hexadecimal int
      - [x] Binary int
      - [x] Float
      - [x] Boolean
      - [x] Char
      - [x] String
      - [x] Raw string
      - [x] Identifier
      - [x] List
    - [x] Operations
      - [x] Basic maths (+, -, *, /, %, ++, --)
      - [x] Logical (==, !=, and, or, not, <, >, <=, >=)
      - [x] Structural (function call, list indexing, member access)
  - [x] Statements
    - [x] Assignement
    - [x] Block
    - [x] Branchs
    - [x] Loops
  - [x] Structure
    - [x] Modules
    - [x] Functions
      - [x] Basic structure
      - [x] Params
      - [x] Return value
      - [x] Returns
      - [x] Visibility
      - [x] Generics
      - [x] Default param values
    - [x] Multi-file support
    - [x] Structs
    - [x] Enums
    - [x] Root statements
- [ ] Semantic analysis
- [ ] Serialization to 3AC
- [ ] Optimization
- [ ] Assembly to custom bytecode
  - [ ] Definition of custom bytecode
  - [ ] Assembler

These side projects are planned for later as well:

- [ ] Interpreter of custom bytecode
- [ ] VSCode extension with a maximum of support for .idk files