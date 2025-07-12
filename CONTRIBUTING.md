<!--
 Copyright 2018-2022 Davide Bettio <davide@uninstall.it>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Contributing

AtomVM is open to any contribution.

[Pull requests](https://github.com/atomvm/AtomVM/pulls),
[bug reports and feature requests](https://github.com/atomvm/AtomVM/issues) are welcome.

However before contributing, please read carefully our [Code of Conduct](CODE_OF_CONDUCT.md) and
the following contribution guidelines.

Please, also make sure to understand the [Apache 2.0 license](LICENSES/Apache-2.0.txt) and the
[Developer Certificate of Origin](https://developercertificate.org/).

Last but not least, **do not use GitHub issues for vulnerability reports**, read instead the
[security policy](SECURITY.md) for instructions.

## Git Recommended Practices

* Commit messages should have a
* [summary and a description](https://github.com/erlang/otp/wiki/writing-good-commit-messages)
* Remove any trailing white spaces
* Always `git pull --rebase`
* [Clean up your branch history](https://git-scm.com/book/id/v2/Git-Tools-Rewriting-History) with
`git rebase -i`
* Squash commits before PR, unless there is a good reason not to
* All your intermediate commits should build

## Coding Style

For all source code modules:

* Remove all trailing whitespace
* Newlines (`\n`) at end of file
* Use line ending conventions appropriate for the platform (e.g., `\n` on UNIX-like systems)

### Copyright Headers

All source code modules should include copyright headers that are formatted for the relevant module language.  Copyright headers should take the following form:

    /*
    * This file is part of AtomVM.
    *
    * Copyright 2020 Your name <your@email.address>
    *
    * Licensed under the Apache License, Version 2.0 (the "License");
    * you may not use this file except in compliance with the License.
    * You may obtain a copy of the License at
    *
    *    http://www.apache.org/licenses/LICENSE-2.0
    *
    * Unless required by applicable law or agreed to in writing, software
    * distributed under the License is distributed on an "AS IS" BASIS,
    * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    * See the License for the specific language governing permissions and
    * limitations under the License.
    *
    * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
    */


### C Code

C source code follows the [AtomVM C Coding Style Guide (AVMCCS)](C_CODING_STYLE.md). The style is enforced with [`clang-format-16`](https://releases.llvm.org/16.0.0/tools/clang/docs/ClangFormat.html). To automatically fix a file, run:

    clang-format-16 --style=file -i file.c

#### Documentation

[Doxygen Javadoc style](https://www.doxygen.nl/manual/docblocks.html) code comments will be picked up and added to the documentation. Changes will automatically be added to the [libAtomVM Source Files](https://doc.atomvm.org/main/apidocs/libatomvm/index.html#libatomvm-source-files) and the [libAtomVM Index](https://doc.atomvm.org/main/apidocs/libatomvm/index.html#libatomvm-index). But to have `Data Structures`, `Types`, `MACROS`, and `Functions` appear in the correct C Library APIs section the corresponding entries must be added to the similarly named `*.rst` files in the `AtomVM/doc/src/apidocs/libatomvm/` directory. The exact names of the files that need to be altered are: `data_structures.rst`, `functions.rst`, `macros.rst`, and `types.rst`. The other files in the directory handle auto`generated content and do not need to be altered.

In the rare case that a function declaration and definition are both in different header files (rather than the definition in a `*.c` file) this can cause rendering errors for `Doxygen`. The work around for these cases can be demonstrated with this example for the function `sys_listener_destroy` it is documented and declared in `sys.h` and defined as follows in `listeners.h`:

    #ifndef DOXYGEN_SKIP_SECTION /* documented in sys.h */
    void sys_listener_destroy(struct ListHead *item)
    {
        EventListener *listener = GET_LIST_ENTRY(item, EventListener, listeners_list_head);
        free(listener);
    }
    #endif /* DOXYGEN_SKIP_SECTION */

> Note: You should include a short `/* comment */` trailing the `#ifndef` entry mentioning the file where the function is actually documented.

### Erlang Code

Erlang source code style is enforced using [erlfmt](https://github.com/WhatsApp/erlfmt).

### Elixir Code

Just use Elixir formatter enforced style.
