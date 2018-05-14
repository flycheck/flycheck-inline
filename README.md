# flycheck-inline

[![License GPL 3](https://img.shields.io/github/license/flycheck/flycheck-inline.svg)][LICENCE]

This is an extension for [Flycheck](http://www.flycheck.org/). It implements a
minor-mode for displaying errors from Flycheck right below their reporting
location, using overlays.

![flycheck-inline warning preview](screenshots/warning.gif)

It also supports displaying *related errors* from Flycheck checkers that support
them.  Here is an example of this behavior for Rust borrowing errors:

![flycheck-inline related errors preview](screenshots/related-errors.gif)

(See also [flycheck-rust][flycheck-rust] for automatically setting up your Rust
projects for Flycheck)

## Installation

Add `flycheck-inline.el` somewhere to your `load-path` and add the following to
your init file:

```emacs-lisp
(with-eval-after-load 'flycheck
  (flycheck-inline-mode))
```

This will turn on inline errors in all buffers after Flycheck has been loaded.

## Configuration

The colors used to display the error/warning/info messages are inherited from
the compilation faces.  You can customize them through `M-x customize-group RET
flycheck-inline RET`.

If you wish to change the delay before errors are displayed, see
`flycheck-display-errors-delay`.

## Contributing

We welcome all kinds of contributions, whether you write patches, open pull
requests, write documentation, help others with Flycheck issues, or just tell
other people about your experiences with Flycheck.  Please take a look at
our [Contributor’s Guide][contrib] for help and guidance about contributing to
Flycheck or to this extension.

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see http://www.gnu.org/licenses/.

[LICENCE]: https://github.com/flycheck/flycheck-licence/blob/master/LICENCE
[contrib]: http://www.flycheck.org/en/latest/contributor/contributing.html
[flycheck-rust]: https://github.com/flycheck/flycheck-rust
