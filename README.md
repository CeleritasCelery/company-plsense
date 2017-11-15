# company-plsense
[![MELPA](https://melpa.org/packages/company-plsense-badge.svg)](https://melpa.org/#/company-plsense)

company-mode completion back-end for Perl using [PlSense](https://github.com/aki2o/plsense).

## Installation

You can install this package from [Melpa](http://melpa.milkbox.net/)
```lisp
M-x package-install RET company-plsense RET
```

## Usage
once installed add the following to your `.emacs`
```lisp
(add-to-list 'company-backends 'company-plsense)
(add-hook 'perl-mode-hook 'company-mode)
(add-hook 'cperl-mode-hook 'company-mode)
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

`company-plsense-ignore-compile-errors`: ignore PlSense errors related to compiling an imported module.

## How do I include external Perl libraries?
PlSense does not currently analyze `use lib` statements so all libraries must
be locatable in either `@INC` or `$PERL5LIB`. If you have a project specific
library use PlSense's [ProjectInfoFile](https://github.com/aki2o/plsense/wiki/Library#projectmodule).

## How do I resolve server problems?
1. Run `company-plsense-executable-version` to verify Emacs can find the executable.
2. The command `company-plsense-server-status` will show the current status of
   the servers. In order to work, all three servers (Main, Work, and Resolve)
   need to be running. If not, start the server with
   `company-plsense-start-server`.
3. If all the servers are running but completion candidates are still not being
   generated, use the command `company-plsense-buffer-ready`. If the buffer
   failed to compile cleanly it will return "Not Found", in which case you will
   need to run `perl -c` on the file to see what the problem is. If the reply is
   "Yes" that means the file loaded properly.
4. If `company-plsense` is still not working, open an issue in the issue tracker.

## Limitations

### No Perl6 support
PlSense only supports Perl5.

### Tied to a file
`company-plsense` can only provide completion candidates for buffers tied to files.

### Clean compile
`PlSense` cannot analyze files that do not compile cleanly. To verify if a file is
free of errors, run `perl -c /path/to/file` or use a linter such as [flycheck](http://www.flycheck.org/en/latest/).
