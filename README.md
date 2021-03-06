# CharLISP: One Character / One Token Language

## Purpose of this project

* To develop a language specification for learning about implementation of minimum interpreter, by using S-expression and one character tokens to avoid lexer/parser complex
* To study or enjoy implementation of Lambda expressions, available to be written in various programming languages, with lexical scope and closure more easily and properly

## Examples

See comments in source codes and examples directory for other examples.

```
('(Hello,World))
=> (H e l l o , W o r l d)

(((:g(gg))(:g(:nr((=n1)(:r)(:((=(((:g(gg))(:g(:nx((=nx)(:1)(:((=(%nx)0)(:(-01))(:((gg)n(+x1)))))))))n2)1)(:((gg)(-n1)($nr)))(:((gg)(-n1)r))))))))(*(*25)(*25))('()))
=> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)

(((:g(gg))(:g(:nr((=n0)(:r)(:((=(%n(*35))0)(:((gg)(-n1)($('(FizzBuzz))r)))(:((=(%n3)0)(:((gg)(-n1)($('(Fizz))r)))(:((=(%n5)0)(:((gg)(-n1)($('(Buzz))r)))(:((gg)(-n1)($nr)))))))))))))(*56)('()))
=> (1 2 (F i z z) 4 (B u z z) (F i z z) 7 8 (F i z z) (B u z z) 11 (F i z z) 13 14 (F i z z B u z z) 16 17 (F i z z) 19 (B u z z) (F i z z) 22 23 (F i z z) (B u z z) 26 (F i z z) 28 29 (F i z z B u z z))
```

## Language specification

* S-expressions by using parentheses and dot for syntax and conscell/list structures, not including space in codes

* Special forms

|Codes|Description|Equivalent in Scheme|
|:---:|:---|:---:|
|`(:abcB)`, `(:B)`|*lambda* syntax with lexical scope|`(lambda (a b c) B)`, `(lambda () B)`|
|`('A)`|*quote* syntax as an abbreviation|`(quote A)`|

* Builtin functions
	* Predicates: `=` `<` (return Church Booleans so *lambda* closures are needed for clauses)
	* Others: `+` `-` `*` `%`(`modulo`) `$`(`cons`) `[`(`car`) `]`(`cdr`)

* Data types: one-character symbols and integers only, except default type of integer as return values

## License

The codes in this repository are licensed under [CC0, Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/).

