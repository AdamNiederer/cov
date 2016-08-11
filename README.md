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
## License
GPLv3
