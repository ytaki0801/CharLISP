# CharLISP: One Character / One Token Interpreters

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

## Language specification overview with Scheme examples

|CharLISP|Scheme||
|---------|------------------------------------|---|
|`($('a)('(bc)))`|`(cons (quote a) (quote (b c)))`|`'` is just an abbreviation of quote, not a reader macro|
|`(^(*(/42)2)4)`|`(expt (* (/ 4 2) 2) 4)`|one digit of intergers only in souce code|
|`(?(%52)0('Y)('N))`|`(if (eq? (modulo 5 2) 0) 'Y 'N)`|equal-only condition syntax|
|`(((:x(:y(-xy)))2)3)`|`(((lambda (x) (lambda (y) (- x y))) 2) 3)`|Lambda expression is also supporting multiple variables and tail call optimization, with no global environment|

## License

The codes in this repository are licensed under [CC0, Creative Commons Zero v1.0 Universal](https://creativecommons.org/publicdomain/zero/1.0/).
