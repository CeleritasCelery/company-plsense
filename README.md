# company-plsense.el 

company-mode completion back-end for Perl using Plsense.

## Installation

clone this repo and add it to your `load-path`. Then add the following to your `.emacs` 
```lisp
(require 'company-plsense)
(company-plsense-setup)

```

## limitations
- only cperl-mode support
- poor error handling
- no perldoc popups yet
