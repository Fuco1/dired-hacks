1. [dired-hacks](#)
2. [Contribute!](#contribute!)
3. [Packages](#packages)
    1. [dired-hacks-utils](#dired-hacks-utils)
    2. [dired-filter](#dired-filter)
    3. [dired-avfs](#dired-avfs)
    4. [dired-open](#dired-open)
    5. [dired-rainbow](#dired-rainbow)
    6. [dired-subtree](#dired-subtree)

# dired-hacks [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C)

Collection of useful dired additions.  I don't want this become
another `dired+`, so I'm splitting all the functionality into separate
mutually independent packages.  All shared functionality and helpers
will be extracted into a single package `dired-hacks-utils`, so that
will be the only dependence.

In addition, all the packages require [dash.el](https://github.com/magnars/dash.el)

<a name="contribute!" />
# Contribute!

Since this collection comes from my own config, it mostly contains
stuff I use or find useful.  If you have a good idea or nice usecase
for some addition, feel free to send patches or start an issue (but
patches are preferred :P).

<a name="packages" />
# Packages

<a name="dired-hacks-utils" />
## dired-hacks-utils

Set of utility functions used in all the `dired-hacks` packages.

<a name="dired-filter" />
## dired-filter

The filtering system is designed after ibuffer: every dired
buffer has associated "filter stack" where user can push
filters (predicates).  These filters are by default
logically "anded", meaning, only the files satsifying all the
predicates are shown.

Some filters take additional input from the user such as part of
name, regexp or extension, other filters only use a predefined
predicate such as "show only directories" or "omit dot files".

In addition, there are two "metafilters", the `or` filter and the
`not` filter.  These take other filters as arguments and change
their logical interpretation.  The `or` filter takes the two
filters on top of the stack, pops them and pushes a filter that
matches files satisfying one or the other (or both) filters.  The
`not` filter pops the top filter and pushes its logical negation.

To enable or disable the filters, toggle minor mode
`dired-filter-mode`.  Toggling this mode preserves the filter
stack, so you can use it to quickly hide/unhide files filtered by
the current filter setup.

All the provided interactive functions are available from
`dired-filter-map`.  You can customize `dired-filter-prefix` to set a
prefix for this map or bind it manually to a prefix of your choice
using:

    (define-key dired-mode-map (kbd "some-key") dired-filter-map)

In addition to filtering, you can also use the same predicates to
only mark files without removing the rest.  All the filtering
functions of the form `dired-filter-by-*` have their marking
counterpart `dired-filter-mark-by-*`.  These are available from
`dired-filter-mark-map`.  You can customize
`dired-filter-mark-prefix` a prefix for this map or bind it
manually to a prefix of your choice using:

    (define-key dired-mode-map (kbd "some-key") dired-filter-mark-map)

The marking operations are not placed on stack, instead, the marks are
immediately updated by "OR"-ing them together.  To remove marks that
would otherwise be selected by a filter, use prefix argument (usually
bound to `C-u`).  To logically negate the meaning of the filter, you
can call the function with a double prefix argument (usually `C-u`
`C-u`)

### Stack operations

To remove the filter from the stack, use `dired-filter-pop` or
`dired-filter-pop-all`

To break a metafilter apart, you can use `dired-filter-decompose`
to decompose the parts of the metafilter and push them back to
the stack.

You can transpose the filters on the top of the stack using
`dired-filter-transpose`

### Built-in filters

Here's a list of built-in filters:

* dired-filter-by-name
* dired-filter-by-regexp
* dired-filter-by-extension
* dired-filter-by-dot-files
* dired-filter-by-omit
* dired-filter-by-predicate
* dired-filter-by-file
* dired-filter-by-directory
* dired-filter-by-mode

You can see their documentation by calling M-x `describe-function`.

Specifically, `dired-filter-by-omit` removes the files that would
be removed by `dired-omit-mode`, so you should not need to use
both---in fact it is discouraged, as it would make the read-in
slower.

To define your own filters, you can use the macro
`dired-filter-define`.  If you define some interesting filter,
please consider contributing it to the upstream.

<a name="dired-avfs" />
## dired-avfs

Adds [avfs](http://avf.sourceforge.net/) support for seamless archive
browsing.  This extension therefore depends on the presence of `avfsd`
on your system.  In debian-derived distributions you can usually do

    apt-get install avfs

`avfs` is probably also available for Mac OS.  You're out of luck on
Windows, sorry.

Once the daemon is installed, run it with `mountavfs` and everything
"Should Just Work™".

<a name="dired-open" />
## dired-open

While emacs already has the `auto-mode-alist`, this is often
insufficient.  Many times, you want to open media files, pdfs or
other documents with an external application.  There's remedy for
that too, namely `dired-guess-shell-alist-user`, but that is still
not as convenient as just hitting enter.

This package adds a mechanism to add "hooks" to `dired-find-file` that
will run before emacs tries its own mechanisms to open the file, thus
enabling you to launch other application or code and suspend the
default behaviour.

By default, two additional methods are enabled,
`dired-open-by-extension` and `dired-open-subdir`.

This package also provides other convenient hooks:

* `dired-open-xdg` - try to open the file using `xdg-open`
* `dired-open-guess-shell-alist` - try to open the file by
  launching applications from `dired-guess-shell-alist-user`
* `dired-open-call-function-by-extension` - call an elisp function
  based on extension.

These are not used by default.

You can customize the list of functions to try by customizing
`dired-open-functions`.

To fall back to the default `dired-find-file`, you can provide the
prefix argument (usually `C-u`) to the `dired-open-file` function.
This is useful for example when you configure html files to be
opened in browser and you want to edit the file instead of view it.

Note also that this package can handle calls when point is not on a
line representing a file---an example hook is provided to open a
subdirectory under point if point is on the subdir line, see
`dired-open-subdir`.

If you write your own handler, make sure they do *not* throw errors
but instead return nil if they can't proceed.  Please, don't forget to
submit interesting handlers!

<a name="dired-rainbow" />
## dired-rainbow

This package adds more customizable highlighting for files in dired
listings.  The group `dired-faces` provides only nine faces and
isn't very fine-grained.

The definitions are added using a macro `dired-rainbow-define`.
You can display its documentation by calling

    M-x describe-function RET dired-rainbow-define RET

Here are some example uses:

```scheme
(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg")
  "Media files.")

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)

; boring regexp due to lack of imagination
(dired-rainbow-define log (:inherit default
                           :italic t) ".*\\.log")
```

<a name="dired-subtree" />
## dired-subtree

The basic command to work with subdirectories in dired is `i`,
which inserts the subdirectory as a separate listing in the active
dired buffer.

This package defines function `dired-subtree-insert` which instead
inserts the subdirectory directly below its line in the original
listing, and indent the listing of subdirectory to resemble a
tree-like structure (somewhat similar to `tree(1)` except the pretty
graphics).  The tree display is somewhat more intuitive than the
default "flat" subdirectory manipulation provided by `i`.

There are several presentation options and faces you can customize
to change the way subtrees are displayed.

You can further remove the unwanted lines from the subtree by using
`k` command or some of the built-in "focusing" functions, such as
`dired-subtree-only-*` (see list below).

If you have the package `dired-filter`, you can additionally filter
the subtrees with global or local filters.

A demo of basic functionality is available on youtube:
https://www.youtube.com/watch?v=z26b8HKFsNE

### Interactive functions

Here's a list of available interactive functions.  You can read
more about each one by using the built-in documentation facilities
of emacs.  It is adviced to place bindings for these into a
convenient prefix key map, for example `C-,`

* `dired-subtree-insert`
* `dired-subtree-remove`
* `dired-subtree-revert`
* `dired-subtree-narrow`
* `dired-subtree-up`
* `dired-subtree-down`
* `dired-subtree-next-sibling`
* `dired-subtree-previous-sibling`
* `dired-subtree-beginning`
* `dired-subtree-end`
* `dired-subtree-mark-subtree`
* `dired-subtree-unmark-subtree`
* `dired-subtree-only-this-file`
* `dired-subtree-only-this-directory`

If you have package `dired-filter`, additional command
`dired-subtree-apply-filter` is available.
