## intp
Imperative and dynamically typed programming language.
### If-else statement
No need to put condition in parentheses
```
if condition {
  print("if block")
} else {
  print("else block")
}
```

### While loop
```
while condition {
  var i = i + 1
}
```
### For loop
```
for var i = 0; i < 10; i += 1 {
  println(i)
}
```

### If-then-else expression
```
if a % 2 == 0 then "even" else "odd"
```

### Data
Key-value table similiar to JS object literal.
```
val Person = data(name, surname)
val john = Person("John", "Smith")
```
Or manually
```
val john = {name = "John", surname = "Smith"}
```
Person actually desugars to function returning object literal i.e. `func(name, surname) {return {name=name, surname=surname}}`

### Function
```
factorial = func(n) {
  if n == 0 or n == 1 return 1
  else return n * factorial(n - 1)
}
```
### Array
`val arr = [1, 2, 3]`

### String
`val a = "test"`

Actually it's syntax sugar for array of characters i.e. `['t', 'e', 's', 't']`

### Other literals
- Integer: `1`
- Double: `3.14`
- Boolean: `true`, `false`
- Character: `'c'`

### Builtin functions
- `println(object)`
- `print(object)`
- `read()` (return single line from stdin)
- `size(array)` (return size when used or arrays, otherwise throws error)
- `assert(condtion)` (when condition is false throws error)
- `exit(code)` (exit program immediately with given status code)
### Casting functions
 --  |  int  |  char | bool | double | string | function | array | object
--- | --- | --- | --- | --- | --- | --- | --- | --- |
**int**  |  yes  |  yes | no | yes | yes | no | no | no |
**char**  |  yes  |  yes | no | yes | yes | no | no | no |
**bool**  |  no  |  no | yes | no | yes | no | no | no |
**double**  |  yes  |  yes | no | yes | yes | no | no | no |
**string**  |  yes  |  yes | yes | yes | yes | yes | yes | yes |
**function** | no | no | no | no | no | yes | no | no |
**array** | no | no | no | no | no | no | yes| no | 
**object** | no | no | no | no | no | no | no | yes | 

### Operators
- Boolean: `not`, `and`, `or`, `==`, `!=`
- Numeric(Double, Integer): `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`
- Char: `==`, `!=`
- Array(String): `+`, `==`, `!=`
- Object: `==`, `!=`
