# 4o# 4or

A language interpreter by Yury Lebedev for the language `4or`, implemented with the guidance of [Crafting Interpreters by Robert Nystrom](https://craftinginterpreters.com/)

# Usage
Run REPL
```
cargo r --bin console
```

Run web REPL (note: you will need to `cd web` before running)
```
trunk serve --open
```

# TODO
- [ ] Remove global variables
- [ ] Allow assignment to expressions
- [x] Remove print statement
- [ ] Empty expressions
- [ ] Replace for loops with iterator loops 
- [ ] fix re-declaration for variables within the same scope (`let a = 1; fn test() { print(a); } let a = 2;` - test should always print 1)


# Language 

- Lexical scoping
