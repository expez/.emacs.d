# An opinionated configuration for The One True Editor

This is my Emacs config.  It is--at all times--just the way I like it.  If you find something in here that you like, I encourage you to make it your own.

If you find something that is broken--I do not use all parts of this configuration at all times!--you can create an issue and watch me solve your problem like it is my own, because tomorrow it might be.

## Usage

All packages are managed through Emacs' own package manager.  There are some exceptions, but I try to keep them to a minimum.  These exceptional packages are included in the `site-lisp` folder, either because they weren't available through the package manager or beacuse I wanted to vendor them so I could make minor changes.

All the customizations, for the various packages, live in files named `init-<package or mode>.el` in the folder `lisp`.  Any file matching this pattern will be automagically loaded.  You can rely on this when installing new packages: create your own `init-<something-new>.el` file with a `(require-package 'something-new)` followed by whatever customizations.

The exception to this rule is the stuff produced by Emacs' customize facilities, they live in the file `customize.el`.

I recommend you start Emacs using `emacs --daemon` and connect to this daemon using `emacsclient`.

## Language support

* Clojure
* Javascript
* Elisp
* C
* The various web languages like HTML/CSS/less/sass

## Installation

Just clone this repository so the file `init.el` ends up at `~/.emacs.d/init.el`.
