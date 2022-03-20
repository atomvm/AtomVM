<!--
 Copyright 2018 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Contributing

Make sure to understand the license and the contribution guidelines before contributing and last but not least be kind.

## Git Recommended Practices

* Commit messages should have a [summary and a description](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
* Avoid trailing white spaces
* Always `git pull --rebase`
* [Clean up your branch history](https://git-scm.com/book/id/v2/Git-Tools-Rewriting-History) with `git rebase -i`
* All your intermediate commits should build

## Coding Style

### C Code

#### Indentation

* [K&R indentation and braces style](https://en.wikipedia.org/wiki/Indentation_style#K&R_style)
* [Mandatory braces](https://en.wikipedia.org/wiki/Indentation_style#Variant:_mandatory_braces)
* 4 spaces indentation

Good:
```
void f(int reverse)
{
    if (reverse) {
        puts("!dlroW olleH");
    } else {
        puts("Hello world");
    }
}
```

Bad:
```
void f(int reverse) {
    if (reverse)
        puts ("!dlroW olleH");
    else
        puts ("Hello world");
}
```

#### Names

* Struct names are PascalCase (e.g. Context)
* Scalar types are lower case (e.g. term)
* All other names (e.g. functions and variables) are snake_case (e.g. term_is_integer)
* Always prefix function names (e.g. term_is_nil, term_is_integer, context_new, context_destroy)

#### Other Coding Conventions
* Pointer * should be with the variable name rather than with the type (e.g. `char *name`, not `char* name`)
* Avoid long lines, use intermediate variables with meaningful names.

### Elixir Code

Just use Elixir formatter enforced style.
