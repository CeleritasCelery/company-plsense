# company-plsense.el 

company-mode completion back-end for Perl using [Plsense](https://github.com/aki2o/plsense).

## Installation

clone this repo then add the following to your `.emacs` 
```lisp
(add-to-list 'load-path "~/path/to/company-plsense")
(require 'company-plsense)
(company-plsense-setup)

```

## Features

### provides completions for
- variables
- Methods
- Modules
- Class Initializers
- LIST of Use/Require statements
- Key of Hashes


### use `company-doc-buffer` to view perldoc and other information

## Configuration
`company-plsense-executable`: location of plsense executable 
`company-plsense-config-path`: location of `.plsense` config file
`company-plsense-ignore-compile-errors`: ignore plsense errors when they releate to compiling a file 

## Limitations

### clean syntax
`plsense` cannot analyze files that do not compile cleanly. To verify if a file is free of errors, run `perl -c /path/to/file`.

### update completion candidates
completion candidates are only updated upon buffer save and open.

### Development in project tree
If you develop in project tree which has particular library for the project, need to make [ProjectInfoFile](https://github.com/aki2o/plsense/wiki/Library#projectmodule).
