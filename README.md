# An opinionated configuration for The One True Editor

This is my Emacs config.  It is--at all times--just the way **I** like it.  If you find something in here that **you** like, I encourage you to make it your own.

If you find something that is broken--I do not use all parts of this configuration at all times!--you can create an issue and watch me solve your problem like it is my own, because tomorrow it might be.

## Usage
All packages are managed through Emacs' own package manager.  There are some exceptions, but I try to keep them to a minimum.  These exceptional packages are managed using the `Rakefile` included in this repository.  Simply run `rake` in the .emacs.d directory and the packages will be installed in the `vendor` folder.

You can also update the packages managed by the rakefile by issuing the command `sh rake update_packages`.  If you want to add packages of your own I strongly suggest you use the built-in package manager.  If you cannot find the packages you want in this manner, modify the rakefile to grab the package you need.  All files below the `vendor` folder will be automagically loaded.

All the customizations, for the various packages, live in files named `init-<package>.el`.  Any file matching this pattern will be automagically loaded.  You can rely on this when installing new packages: add the package to the list of packages to be installed (in the file `init-package.el`) and create your own `init-<my-package>.el` file with the appropriate customizations.

The exception to this rule is the stuff produced by Emacs customize facilities, they live in the file `customize.el`.  Settings affecting Emacs proper live toward the bottom of the file `init.el`.

## Language support
At present, or in the recent past, I have taken the time to setup Emacs for the following languages:

* Haskell
* Common lisp
* Ruby
* C
* Java
* Elisp

## Installation
Just clone this repository so the file `init.el` ends up at `~/.emacs.d/init.el`.  If you care for the packages found in the `Rakefile` additionally run ` rake` in the .emacs.d directory and occasionally run `rake update_packages` to update the packages occasionally.

I recommend you start Emacs using `emacs --daemon` and connect to this daemon using `emacsclient`.
