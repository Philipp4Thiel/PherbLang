# PherbLang

ok these bug-chainreactions are getting annoying, time to actually plan ahead.

# Lets plan ahead this time:
1. Parsing `.pherb` files into a first AST
2. parsing that AST into a second AST with more information
3. do typechecking on that AST
4. evaluate that AST
5. might compile that AST to bytecode

# Parsing `.pherb` files into a first AST
- [ ] `Lexer` class (given a file it should return a token stream)
- [ ] `Parser` class (given a token stream it should return an AST)

## Grammer of PherbLang
```
program = { statement }
 
statement = declaration | assignment | printStatement ";"

declaration = "let" IDENTIFIER ":" type "=" expression 
assignment = IDENTIFIER "=" expression
printStatement = "print" expression

expression = literal | unary | binary | grouping | IDENTIFIER 
                | lambda | application | "(" expression ")" | conditional

literal = NUMBER
unary = ("-" | "!") expression
binary = expression operator expression
grouping = "(" expression ")"
lambda = ("\" IDENTIFIER { "," IDENTIFIER } "->" expression)
application = expression expression { expression }
conditional = expression "?" expression ":" expression

operator = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

type = "int" | type "->" type | "(" type ")"
```

example would be:
```
let x: int = 5;
let y: int = 10;
let add: int -> int -> int = (\a, b -> a + b);
print add x y;

let add5: int -> int = add 5;
print add5 10;

let fib: int -> int = (\n -> n < 2 ? n : fib (n - 1) + fib (n - 2));
print fib 10;

let idk: (int -> int) -> int = (\f -> f 5);
print idk (\x -> x + 5);
```

# Parsing that AST into a second AST with more information
- [ ] annotate the AST with basic type information
- [ ] annotate the AST with captures for closures

# Do typechecking on that AST
- [ ] check if there are any type errors

# Evaluate that AST
- [ ] evaluate the AST

# Might compile that AST to bytecode
- [ ] idk will worry about this later