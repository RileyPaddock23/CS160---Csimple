# Csimple Language Reference

Csimple is a simple, statically-typed procedural language. This document describes its syntax and semantics.

---

## Program Structure

A Csimple program is a collection of **procedures**. Every valid program must contain a procedure named `Main` that takes no arguments.

```
procedure Main() return <type> {
    // optional nested procedure declarations
    // variable declarations
    // statements
    return <expr>;
}
```

Procedures may be nested—a procedure can be declared inside another procedure's body before any variable declarations.

---

## Data Types

| Type         | Description                              |
|--------------|------------------------------------------|
| `integer`    | Signed integer                           |
| `char`       | Single character                         |
| `boolean`    | Boolean value (`true` or `false`)        |
| `string[N]`  | Fixed-size character array of length `N` |
| `intptr`     | Pointer to an integer                    |
| `charptr`    | Pointer to a character                   |

> **Note:** `string[N]` is only allowed as a variable type, not as a procedure parameter or return type.

---

## Literals

| Literal kind    | Syntax examples                         |
|-----------------|-----------------------------------------|
| Decimal integer | `0`, `42`, `1024`                       |
| Hexadecimal     | `0xFF`, `0x1A`                          |
| Octal           | `07`, `0755`                            |
| Binary          | `1010b`, `11b`                          |
| Character       | `'a'`, `'Z'`, `'0'`                     |
| Boolean         | `true`, `false`                         |
| String          | `"hello, world"`                        |
| Null pointer    | `null`                                  |

---

## Operators

### Arithmetic

| Operator | Description    | Types          |
|----------|----------------|----------------|
| `+`      | Addition       | `integer`      |
| `-`      | Subtraction    | `integer`      |
| `*`      | Multiplication | `integer`      |
| `/`      | Division       | `integer`      |
| `-x`     | Unary negation | `integer`      |
| `\|x\|`  | Absolute value | `integer`      |

### Comparison

All comparison operators produce a `boolean` result.

| Operator | Description              |
|----------|--------------------------|
| `==`     | Equal                    |
| `!=`     | Not equal                |
| `<`      | Less than                |
| `>`      | Greater than             |
| `<=`     | Less than or equal to    |
| `>=`     | Greater than or equal to |

### Logical

| Operator | Description | Types     |
|----------|-------------|-----------|
| `&&`     | Logical AND | `boolean` |
| `\|\|`   | Logical OR  | `boolean` |
| `!`      | Logical NOT | `boolean` |

### Pointer Operators

| Operator   | Description                         | Example          |
|------------|-------------------------------------|------------------|
| `&var`     | Address-of (reference)              | `p = &x;`        |
| `^expr`    | Dereference                         | `v = ^p;`        |

---

## Operator Precedence (highest to lowest)

| Level | Operators                          |
|-------|------------------------------------|
| 1     | `!`, unary `-`, `^`, `&`          |
| 2     | `*`, `/`                           |
| 3     | `+`, `-`                           |
| 4     | `==`, `!=`, `<`, `>`, `<=`, `>=`  |
| 5     | `&&`                               |
| 6     | `\|\|`                             |

---

## Declarations

### Variable Declaration

```
var <identifier-list> : <type> ;
```

Multiple variables of the same type can be declared in one statement:

```
var x, y, z : integer;
var flag     : boolean;
var name     : string[20];
var p        : intptr;
```

Variable declarations must appear before any statements in a procedure body.

### Procedure Declaration

```
procedure <name>(<param-list>) return <type> {
    // nested procedure declarations
    // variable declarations
    // statements
    return <expr>;
}
```

#### Parameter List

Parameters are grouped by type and separated by semicolons:

```
procedure Add(a, b : integer; flag : boolean) return integer {
    ...
}
```

Parameters may only use non-string types (`integer`, `char`, `boolean`, `intptr`, `charptr`).

---

## Statements

### Assignment

```
<lhs> = <expr> ;
<lhs> = "<string-literal>" ;
```

Left-hand sides can be:
- A plain variable: `x = 5;`
- An array element: `name[0] = 'H';`
- A dereferenced pointer: `^p = 10;`

### Procedure Call

```
<lhs> = <procedure-name>(<arg-list>) ;
```

The return value of the called procedure is assigned to the left-hand side.

```
result = Add(3, 4);
```

### If Statement

```
if (<boolean-expr>) {
    <code-block>
}

if (<boolean-expr>) {
    <code-block>
} else {
    <code-block>
}
```

### While Loop

```
while (<boolean-expr>) {
    <code-block>
}
```

### Code Block

A block of declarations and statements enclosed in braces can appear anywhere a statement is expected:

```
{
    var temp : integer;
    temp = x;
    x = y;
    y = temp;
}
```

### Return Statement

Every procedure body ends with exactly one return statement:

```
return <expr> ;
```

---

## Comments

Csimple uses block comments only. There are no line comments.

```
/% This is a comment %/

/%
   Multi-line
   comment
%/
```

---

## Complete Example

```
/% Compute the factorial of n recursively %/
procedure Factorial(n : integer) return integer {
    var result : integer;
    if (n <= 1) {
        result = 1;
    } else {
        result = n * Factorial(n - 1);
    }
    return result;
}

procedure Main() return integer {
    var answer : integer;
    answer = Factorial(5);
    return answer;
}
```
