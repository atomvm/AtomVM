<!--
 Copyright 2019-2022 Fred Dushin <fred@dushin.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM Documentation

AtomVM documentation is built using the following tools:

* Sphinx, for main HTML, PDF, and ePUB documentation
* Doxygen, for AtomVMLib API documentation
* Rebar3, for Erlang library documentation (requires active internet connection during build to retrieve tool dependancies)
* Graphviz, for image generation

Dependencies and make files are generated via the standard CMake tooling used in AtomVM builds.  However, the documentation sets are not built by default.  Instead, issue the following make targets after a CMake

* `make sphinx-html` to build the Sphinx HTML documentation (includes libAtomVM 'C' and Erlang APIs)
* `make sphinx-epub` to build the Sphinx ePUB documentation, (includes Erlang APIs)
* `make doc` to build all of the above


## Sphinx

To build documentation using Sphinx, we recommend using a Python virtual environment.

    shell$ python3 -m venv $HOME/python-env/sphinx
    shell$ . $HOME/python-env/sphinx/bin/activate

    (sphinx) shell$ python3 -m pip install sphinx
    ...
    (sphinx) shell$ python3 -m pip install myst-parser
    ...
    (sphinx) shell$ python3 -m pip install sphinx-rtd-theme
    ...
    (sphinx) shell$ python3 -m pip install gitpython
    ...
    (sphinx) shell$ python3 -m pip install breathe
    ...
    (sphinx) shell$ python3 -m pip install pygments


There is also a reqirements.txt to simplify installing all of the dependencies at once.

    shell$ python3 -m venv $HOME/python-env/sphinx
    shell$ . $HOME/python-env/sphinx/bin/activate

    (sphinx) shell$ python3 -m pip install -r requirements.txt
