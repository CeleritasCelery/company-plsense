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

`company-plsense-ignore-compile-errors`: ignore PlSense errors related to compiling an imported module.

## How do I verify the PlSense server is setup?
(note all of these steps are optional except the first one)
1. Download PlSense from [github](https://github.com/aki2o/plsense) and follow the
   installation steps.
2. Pick a test file to verify config is correct.
3. Start PlSense with `plsense -i`, which should start an interactive PlSense session.
4. Start the PlSense server with `> serverstart`
5. Verify server started with `> serverstatus` it should show all servers running.
6. Open your test file with `> open /path/to/test/file`
7. verify that the file is loaded with `> ready /path/to/test/file`. If
   everything went well it should reply `Yes`.
8. Attempt file completion by giving a partial symbol name `> assist $foo`. If
   you file had a variable called `$foo_bar` you should see that suggestion in
   the reply.
9. close the server with `> serverstop` and `> exit`.
10. You're good to go!

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

### Tied to a file
`company-plsense` can only provide completion candidates for buffers tied to files.

### Clean compile
`PlSense` cannot analyze files that do not compile cleanly. To verify if a file is
free of errors, run `perl -c /path/to/file`.

### Update completion candidates
completion candidates are only updated upon buffer save and open.
