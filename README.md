# Nolfaris

## Description

Nolfaris is a command line utility which can be used in order to compile the NoKe programming language.

NoKe is a strictly typed programming language designed to be used with the Arcadia Engine as a scripting language.

The compiler is written in Rust and uses Pest as a parser generator.

## Roadmap

Here is a roadmap of the planned features:

- [ ] Parser
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
  - [ ] Statements
    - [x] Assignement
    - [ ] Block
    - [ ] Branchs
    - [ ] Loops
  - [ ] Structure
    - [ ] Functions
    - [ ] Multi-file support
    - [ ] Structs
    - [ ] Enums
    - [ ] ... TBD
- [ ] Semantic analysis
- [ ] Serialization to 3AC
- [ ] Optimization
- [ ] Assembly to custom bytecode
  - [ ] Definition of custom bytecode
  - [ ] Assembler
- [ ] Interpreter of custom bytecode