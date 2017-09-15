# company-plsense 

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
`company-plsense-executable`: location of PlSense executable 

`company-plsense-config-path`: location of `.plsense` config file

`company-plsense-ignore-compile-errors`: ignore PlSense errors when they releate to compiling a file 

## How do I setup the PlSense server?
1. Download PlSense from [github](https://github.com/aki2o/plsense) and follow the installation steps.
2. Pick a test file to verify config is correct.
3. Start PlSense with `PlSense -i`, which should start an interactive PlSense session.
4. Start the PlSense server with `> serverstart`
5. Verify server started with `> serverstatus` it should show all work servers running.
6. Open your test file with `> open /path/to/test/file`
7. verify that the file is loaded with `> ready /path/to/test/file`. If everything went well it should reply `Yes`.
8. Attempt file completion by giving a partial symbol name `> assist $foo`. If you file had a variable called `$foo_bar` you should see that suggestion in the reply.
9. close the server with `> serverstop` and `> exit`.
10. You're good to go!

## How do I include external Perl libraries?
PlSense does not currently analyze `use lib` statements so all libraries must be locatable in either `@INC` or `$PERL5LIB`. 
If you have a project specific library use PlSense's [ProjectInfoFile](https://github.com/aki2o/plsense/wiki/Library#projectmodule).

## Limitations

### clean syntax
`PlSense` cannot analyze files that do not compile cleanly. To verify if a file is free of errors, run `perl -c /path/to/file`.

### update completion candidates
completion candidates are only updated upon buffer save and open.
