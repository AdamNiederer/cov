![GPLv3](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)
[![Build Status](https://travis-ci.org/AdamNiederer/cov.svg?branch=master)](https://travis-ci.org/AdamNiederer/cov)
[![MELPA](http://melpa.org/packages/cov-badge.svg)](http://melpa.org/#/cov)

[`gcov`]: https://gcc.gnu.org/onlinedocs/gcc/Gcov.html
 "gcov — a Test Coverage Program"
[`lcov`]: https://github.com/linux-test-project/lcov
 "lcov — a graphical frontend for gcov"
[`coverage.py`]: https://coverage.readthedocs.io
 "Coverage.py is a tool for measuring code coverage of Python programs."
[`clover`]: https://openclover.org
 "OpenClover — Java, Groovy and AspectJ code coverage tool"
[`Coveralls`]: https://coveralls.io
 "Coveralls — Test Coverage History & Statistics"
[`undercover.el`]: https://github.com/sviridov/undercover.el
 "undercover.el: A test coverage library for Emacs"
[MELPA]: https://melpa.org/#/cov "cov @ MELPA"


# cov

`cov` shows code coverage data for your program in emacs. Currently,
it supports [`gcov`], [`lcov`], [`coverage.py`], and [`clover`]
output, as well as the [`Coveralls`] format produced by
[`undercover.el`].

![Screenshot](example.png)

## Installation
`cov` is on [MELPA]. To install it, type `M-x package-install RET cov RET`

## Usage
Enable `cov-mode` in a buffer. It will use the methods in
`cov-coverage-file-paths` to look for a suitable coverage data file.
`cov-mode` uses file watchers to automatically reload the coverage
file when it has been updated.

Line coverage is displayed in the left fringe and added to the
`help-echo` text property of the line.

## Customization

### Profiling mode

By default, `cov` will run in profiling mode. Lines which are executed
a lot will have the fringe decorated with `cov-heavy-face`, while
lines executed less will have the fringe decorated with `cov-med-face`
and `cov-light-face`. Lines not executed at all will be decorated with
`cov-none-face`.

Use the `cov-high-threshold` and `cov-med-threshold` variables to
customize where the limits between haevy, medium, and light execution
should be.

### Coverage mode

Setting `cov-coverage-mode` to `t` *before enabling `cov-mode`* will
run cov in coverage mode. In this mode lines will be marked with
`cov-coverage-run-face` or `cov-coverage-not-run-face` to executed and
non-executed lines respectively. Coverage mode should make finding
uncovered lines slightly easier than profiling mode.


### Finding The Coverage File

Coverage files are located using the methods in
`cov-coverage-file-paths`, a list of paths or functions that are tried
in order to find a matching coverage file for the current buffer.

Paths can be used for coverage files which have the same name as the
source file but with a suffix added, like `gcov`.  So far `gcov` is
the only supported tool that uses this scheme, but more can be added
by appending to `cov-coverage-alist`.  `cov-coverage-alist` bind
postfixes to the coverage tools:

```lisp
(setq cov-coverage-alist '((".gcov" . gcov)))
```
For other tool or more complex environments it is also possible to provide a
function instead of a path string. The function will be called with
the path and name of the buffer file and should return a cons cell of
the form `(COV-FILE-PATH . COVERAGE-TOOL)`. `PATH` shall be the full path
and name of the coverage data file. `COVERAGE-TOOL` shall specify the
coverage tool.

#### Finding gcov Files

`gcov` files are located using `cov-coverage-file-paths` as described
above.

#### Finding lcov Files

An `lcov` tracefile can contain data for multiple source files and can
be named anything. There are two ways to find tracefiles.

1. Set `cov-lcov-file-name` to the absolute path of the tracefile.
   This can be done with a directory- or file-local variable.

2. Add pattarns or functions to `cov-lcov-patterns`.
   Relative or absolute file patterns like "../lcov.info" will be
   tested in order to find a matching file. Functions takes the dir
   and name of the source file as arguments and should return an
   absolute path.

#### Finding coveralls Files

Coveralls file are named `coverage-final.json` and are searched for in
each directory from the source file to the root.

#### Finding clover Files

Clover file are named `clover.xml` and are searched for in
each directory from the source file to the root.

#### Finding coverage.py Files

`coverage.py` file are named `coverage.json` and are searched for in
each directory from the source file to the root.

## Develop

`cov.el` can be extended to understand more coverage formats. To add
a new format:

1. Add a function to `cov-coverage-file-paths` that locates a coverage
   file for a given file. If a coverage file is found, it should
   return a cons of the coverage file path and an identifier, like
   `(cons filepath 'mytool)`. The original name of the file can be
   found in the `cov-coverage-file` buffer local variable.
2. Implement a `cov--mytool-parse`. The parse function is called with
   a temp buffer with the coverage file data as `(current-buffer)` and
   should parse the data and return the coverage as a alist of files
   to coverage mapping. Coverage data is simply a list of two element
   lists, where the first element is the line number and the second
   the coverage count.

### Test
Install dependencies:
```bash
cask install
```

Run tests:
```bash
cask exec ert-runner
```

## License
GPLv3+
