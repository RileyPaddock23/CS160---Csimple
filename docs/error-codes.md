# Csimple Compiler Error Codes

When the Csimple compiler encounters a semantic error it prints a message to **stderr** and exits with a non-zero status code. Syntax errors (from the parser) are printed to stderr but do not produce a specific exit code beyond `1`.

---

## Exit Codes

| Exit Code | Error Name                  | Meaning                                                                                  |
|-----------|-----------------------------|------------------------------------------------------------------------------------------|
| 0         | *(success)*                 | Compilation succeeded; DOT output written to stdout.                                     |
| 2         | `no_main`                   | No procedure named `Main` was found in the program.                                      |
| 3         | `nonvoid_main`              | The `Main` procedure has one or more parameters (it must take no arguments).             |
| 4         | `dup_proc_name`             | Two procedures with the same name are declared in the same scope.                        |
| 5         | `dup_var_name`              | Two variables with the same name are declared in the same scope.                         |
| 6         | `proc_undef`                | A call was made to a procedure that has not been declared.                               |
| 7         | `var_undef`                 | A variable was referenced that has not been declared in any enclosing scope.             |
| 8         | `narg_mismatch`             | A procedure call passes a different number of arguments than the declaration expects.    |
| 9         | `arg_type_mismatch`         | The type of one or more arguments does not match the corresponding parameter type.       |
| 10        | `ret_type_mismatch`         | The type of the return expression does not match the procedure's declared return type.   |
| 11        | `call_type_mismatch`        | Type mismatch in the arguments of a procedure call (more specific call-site check).     |
| 12        | `ifpred_err`                | The predicate (condition) of an `if` statement is not of type `boolean`.                 |
| 13        | `whilepred_err`             | The predicate (condition) of a `while` statement is not of type `boolean`.               |
| 14        | `array_index_error`         | The index expression used to access an array/string element is not of type `integer`.   |
| 15        | `no_array_var`              | An attempt was made to index a variable that is not of type `string`.                   |
| 16        | `incompat_assign`           | The type of the right-hand-side expression does not match the left-hand-side variable.  |
| 17        | `expr_type_err`             | An operator was applied to operands of incompatible types.                               |
| 18        | `expr_pointer_arithmetic_err` | Arithmetic was performed on a pointer type, which is not permitted.                    |
| 19        | `expr_abs_error`            | The absolute-value operator `|x|` was applied to a non-integer expression.              |
| 20        | `expr_addressof_error`      | The address-of operator `&` was applied to an invalid target.                            |
| 21        | `invalid_deref`             | The dereference operator `^` was applied to an expression that is not a pointer.        |

---

## Error Message Format

Error messages are printed to **stderr** in the following format:

```
on line number <N>, error: <description>
```

For example:

```
on line number 7, error: undefined variable
```

---

## Common Mistakes and How to Fix Them

### Exit 2 — No `Main`

Every Csimple program must define a procedure with the exact name `Main`:

```
/% Wrong: lowercase %/
procedure main() return integer { ... }

/% Correct %/
procedure Main() return integer { ... }
```

### Exit 3 — `Main` Has Parameters

```
/% Wrong %/
procedure Main(n : integer) return integer { ... }

/% Correct %/
procedure Main() return integer { ... }
```

### Exit 4/5 — Duplicate Names

A procedure or variable cannot share a name with another declaration in the same scope:

```
var x : integer;
var x : boolean;   /% Error: dup_var_name (exit 5) %/
```

### Exit 12/13 — Non-Boolean Predicate

The condition of `if` and `while` must evaluate to `boolean`:

```
var n : integer;

/% Wrong: integer used as predicate %/
while (n) { ... }

/% Correct %/
while (n != 0) { ... }
```

### Exit 14 — Non-Integer Array Index

```
var name : string[10];
var i    : boolean;

name[i] = 'x';  /% Error: array index must be integer (exit 14) %/
```

### Exit 21 — Invalid Dereference

```
var x : integer;
var v : integer;

v = ^x;   /% Error: x is not a pointer (exit 21) %/

/% Correct: use a pointer type %/
var p : intptr;
p = &x;
v = ^p;
```
