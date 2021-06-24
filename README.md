# CharLISP: One Character / One Token Language

## Purpose of this project

* To develop a language specification to study implementation of minimum interpreter, available to be written in various programming languages, by using S-expression and one character tokens to avoid lexer/parser complex
* To study or enjoy implementation of Lambda expressions with lexical scope and closure more easily and properly

## Examples

See comments in source codes for other examples.

```
('(Hello,World))
=> (H e l l o , W o r l d)

(((:g(gg))(:g(:nr(?n1r(?(((:g(gg))(:g(:nx(?nx1(?(%nx)0(-01)((gg)n(+x1)))))))n2)1((gg)(-n1)($nr))((gg)(-n1)r))))))(^(*25)2)('()))
=> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)

(((:g(gg))(:g(:nr(?n0r(?(%n(*35))0((gg)(-n1)($('(FizzBuzz))r))(?(%n3)0((gg)(-n1)($('(Fizz))r))(?(%n5)0((gg)(-n1)($('(Buzz))r))((gg)(-n1)($nr)))))))))(*56)('()))
=> (1 2 (F i z z) 4 (B u z z) (F i z z) 7 8 (F i z z) (B u z z) 11 (F i z z) 13 14 (F i z z B u z z) 16 17 (F i z z) 19 (B u z z) (F i z z) 22 23 (F i z z) (B u z z) 26 (F i z z) 28 29 (F i z z B u z z))
```
## Language specification

* S-expressions by using parentheses for syntax and list structures, not including space or dot notation

* Special forms

|CharLISP||Scheme correspondence|
|---|---|---|
|`(:abcB)`|lambda syntax with lexical scope|`(lambda (a b c) B)`|
|`(?ABYZ)`|if syntax with equal-only condition|`(if (eq? A B) Y Z)`|
|`('A)`|quote syntax as an abbreviation|`(quote A)`|

* Builtin functions

|CharLISP|Scheme|
|---|---|
|`+` `-` `*` `/`|`+` `-` `*` `/'|
|`%`|`modulo`|
|`^`|`expt`|
|`$`|`cons`|

* Data types
	* One-character symbols and integers only in codes
	* Default type of integer as return values of integer

## License

The codes in this repository are licensed under [CC0, Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/).

