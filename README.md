# cov
`cov` shows code coverage data for your program in emacs. Currently, it only supports gcov output.
![Screenshot](example.png)
## Installation
`cov` is not yet available on MELPA, but it might be some day.
For now, load it in your init script
`(load-file "/path/to/cov")`
## Usage
`set-overlays` and `clear-overlays` will add and remove the data overlays. Proper namespacing and a mode are to be implemented.
This program assumes you have a standard gcov file for your current buffer. For example, if your current buffer is `/whatever/strings.c`, `cov` assumes there's a file called `/whatever/strings.c.gcov`
## Customization
Soon! Nothing is hardcoded right now, so don't be scared to change any of the constants

### Coverage File
The coverage tool adds a postfix to the source file name to store the coverage data. For example, `gcov` adds `.gcov` to the file name. This postfix is the default that `gcov-mode` uses in order to locate the data.
You can customize this default by setting the alist `gcov-coverage-alist`, which bind postixes the coverage tools:
```lisp
(setq gcov-coverage-alist '((".gcov" . 'gcov)))
```

If the coverage file is not stored in the same directory as the source file, the list `gcov-coverage-file-paths` can be set to contain additional paths, relative to the source path, to search. For example, with this configarion the current directory and the subdirectory `cov` will be used:
```lisp
(setq gcov-coverage-file-paths '("." "cov")))
```

To set the variable to project specific values, e.g. in `.dir-locals.el` file, you can make that variable buffer local by adding this to your init.el:

```lisp
(make-variable-buffer-local 'gcov-coverage-file-paths)
```

For more complex environments it is also possible to provide a function instead ot a path string. The function will be called with the path and name of the buffer file and should return a cons cell of the form (PATH . COVERAGE-TOOL). PATH shall be the full path and name of the coverage data file. COVERAGE-TOOL shall specify the coverage tool.

## License
GPLv3
