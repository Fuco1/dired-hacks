* [dired-hacks](#dired-hacks--)
* [Contribute!](#contribute)
* [Packages](#packages)
   * [dired-hacks-utils](#dired-hacks-utils)
   * [dired-filter](#dired-filter)
      * [Stack operations](#stack-operations)
      * [Built-in filters](#built-in-filters)
      * [Saved filters](#saved-filters)
      * [Filter groups](#filter-groups)
      * [Other features](#other-features)
   * [dired-avfs](#dired-avfs)
   * [dired-open](#dired-open)
   * [dired-rainbow](#dired-rainbow)
   * [dired-subtree](#dired-subtree)
      * [Interactive functions](#interactive-functions)
   * [dired-ranger](#dired-ranger)
      * [Multi-stage copy/pasting of files](#multi-stage-copypasting-of-files)
      * [Bookmarks](#bookmarks)
   * [dired-narrow](#dired-narrow)
   * [dired-list](#dired-list)
   * [dired-collapse](#dired-collapse)

# dired-hacks [![Build Status](https://travis-ci.org/Fuco1/dired-hacks.svg?branch=master)](https://travis-ci.org/Fuco1/dired-hacks) [![Paypal logo](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=CEYP5YVHDRX8C)

Collection of useful dired additions.  I don't want this become
another `dired+`, so I'm splitting all the functionality into separate
mutually independent packages.  All shared functionality and helpers
will be extracted into a single package `dired-hacks-utils`, so that
will be the only dependence.

In addition, all the packages require [dash.el](https://github.com/magnars/dash.el)

Please note that only the packages that are listed in this readme are
"finished" (means in package repositories, with usable UI etc.).  All
the other files are work-in-progress packages you could probably use,
but it would be a bit more painful.

# Contribute!

If you want to support this project, you can do it in the following ways:

* Contribute code. Since this collection comes from my own config, it
  mostly contains stuff I use or find useful.  If you have an idea
  that is not yet implemented and will benefit this project, feel free
  to implement it and submit a pull request. If you have any concerns
  whether your contribution will be accepted, ask beforehand. You can
  email the author or
  [start an issue](https://github.com/Fuco1/dired-hacks/issues/new) on
  the tracker.
* Contribute ideas. Even if you can't code Emacs Lisp, you can still
  contribute valuable ideas for other programmers to implement. Simply
  [start new issue](https://github.com/Fuco1/dired-hacks/issues/new)
  on the tracker and submit your suggestion.
* You can make a
  [financial donation](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=TAWNECQR3TTUY)
  through PayPal.  If you like dired-hacks and can spare a modest
  amount on a donation, feel free to do so.  These donations are
  expressions of your gratitude and are used for my personal "rewards"
  (books, games, music etc.).  You can also gift me a game on
  [Steam](http://steamcommunity.com/profiles/76561198265034071/wishlist)
  or buy something on
  [Amazon](http://www.amazon.com/gp/registry/wishlist/2I8DOQH8OZEUR).
  Regardless of the donations, dired-hacks will always be free both as
  in beer and as in speech.

# Packages

## dired-hacks-utils

Set of utility functions used in all the `dired-hacks` packages.

This package also provides these interactive functions:

* `dired-hacks-next-file` - go to next file, skipping empty and non-file lines
* `dired-hacks-previous-file` - go to previous file, skipping empty
  and non-file lines

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

The bindings follow a convention where the filters are mapped on
lower-case letters or punctuation, operators are mapped on symbols
(such as !, |, * etc.)  and group commands are mapped on upper-case
letters.  The exception to this is `p` which is bound to
`dired-filter-pop`, which is a very common operation and warrants a
quick binding.

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

You can use saved filters to mark files by calling
`dired-filter-mark-by-saved-filters`.

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

* `dired-filter-by-name`
* `dired-filter-by-regexp`
* `dired-filter-by-extension`
* `dired-filter-by-dot-files`
* `dired-filter-by-omit`
* `dired-filter-by-garbage`
* `dired-filter-by-predicate`
* `dired-filter-by-file`
* `dired-filter-by-directory`
* `dired-filter-by-mode`
* `dired-filter-by-symlink`
* `dired-filter-by-executable`

You can see their documentation by calling M-x `describe-function`.

Specifically, `dired-filter-by-omit` removes the files that would
be removed by `dired-omit-mode`, so you should not need to use
both---in fact it is discouraged, as it would make the read-in
slower.

When called with negative prefix argument, some filters can read
multiple values.  The resulting predicate is often much faster than
having the filter repeated with single argument.  Read the
documentation to learn more about the calling conventions.
Currently, these filters support reading multiple arguments:

* `dired-filter-by-extension`

To define your own filters, you can use the macro
`dired-filter-define`.  If you define some interesting filter,
please consider contributing it to the upstream.

### Saved filters

In addition to the built-in filters and your own custom filters,
this package provides an option to save complex compound filters
for later use.  When you set up a filter stack you would like to
save, call `dired-filter-save-filters`.  You will be prompted for a
name under which this stack will be saved.

The saved filter will be added to `dired-filter-saved-filters`
variable, which you can also customize via the customize interface
or manually add entries with `push` or `add-to-list`.  If you use
customize, calling `dired-filter-save-filters` will automatically
save the new value into your customize file.

You can delete saved filters with `dired-filter-delete-saved-filters`.

To use a saved filter, you can use either
`dired-filter-add-saved-filters` or
`dired-filter-load-saved-filters`.  The first pushes the saved
filter on top of the currently active stack, the second clears
current filter stack before loading the saved filter configuration.

An example use is to create filters for "logical groups" of files,
such as media files, image files or files used when programming in
certain environment (for example, show files with .h and .c
extensions).  Saved filters save you the time of setting up the
filters each time you want this specific view.

As a concrete example of above, author uses a saved filter "media"
with value:

    (extension "ogg" "flv" "mpg" "avi" "mp4" "mp3")
    ;; show all files matching any of these extensions

### Filter groups

Furthermore, instead of only filtering the dired buffer by
removing lines you are not interested in, you can also group
lines together by filters.  That is, lines (files,
directories...) satisfying a filter will be moved together under
a common drawer.  This mechanism works in analogy with ibuffer
filter groups.

The variable `dired-filter-group-saved-groups` contains
definitions of filter groups.  You can create and save multiple
filter groups (views) and switch between them by setting the
`dired-filter-group` variable.

To enable or disable the filter groups toggle minor mode
`dired-filter-group-mode`.  Toggling this mode preserves the active
filter group so you can use it to quickly group and ungroup the
files.

Here is a screenshot with an active filter group.  Notice that regular
filtering works also with filter groups.

![Filter group](http://i.imgur.com/qtiDX1c.png)

Placing the point on the drawer header and hitting `RET` folds it.
Hitting `RET` again expands it.

![Folding](http://i.imgur.com/TDUsEKq.png)

The `dired-filter-group-saved-groups` used in the above screenshot is the following:

```elisp
(("default"
  ("PDF"
   (extension . "pdf"))
  ("LaTeX"
   (extension "tex" "bib"))
  ("Org"
   (extension . "org"))
  ("Archives"
   (extension "zip" "rar" "gz" "bz2" "tar"))))
```

You can of course be more imaginative and use filtering based on other
criteria than just extensions ;)

### Other features

You can clone the currently visible dired buffer by calling
`dired-filter-clone-filtered-buffer`.

## dired-avfs

Adds [avfs](http://avf.sourceforge.net/) support for seamless archive
browsing.  This extension therefore depends on the presence of `avfsd`
on your system.  In debian-derived distributions you can usually do

    apt-get install avfs

`avfs` is probably also available for Mac OS.  You're out of luck on
Windows, sorry.

Once the daemon is installed, run it with `mountavfs` and everything
"Should Just Work™".

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

## dired-rainbow

This package adds more customizable highlighting for files in dired
listings.  The group `dired-faces` provides only nine faces and
isn't very fine-grained.

The definitions are added by several macros, currently available
are:

* `dired-rainbow-define` - add face by file extension
* `dired-rainbow-define-chmod` - add face by file permissions

You can display their documentation by calling (substituting the
desired macro name):

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

; highlight executable files, but not directories
(dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*")
```

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

## dired-ranger

This package implements useful features present in the
[ranger](http://ranger.nongnu.org/) file manager which are missing
in dired.

### Multi-stage copy/pasting of files

A feature present in most orthodox file managers is a "two-stage"
copy/paste process.  Roughly, the user first selects some files,
"copies" them into a clipboard and then pastes them to the target
location.  This workflow is missing in dired.

In dired, user first marks the files, then issues the
`dired-do-copy` command which prompts for the destination.  The
files are then copied there.  The `dired-dwim-target` option makes
this a bit friendlier---if two dired windows are opened, the other
one is automatically the default target.

With the multi-stage operations, you can gather files from
*multiple* dired buffers into a single "clipboard", then copy or
move all of them to the target location.  Another huge advantage is
that if the target dired buffer is already opened, switching to it
via ido or ibuffer is often faster than selecting the path.

Call `dired-ranger-copy` to add marked files (or the file under
point if no files are marked) to the "clipboard".  With non-nil
prefix argument, add the marked files to the current clipboard.

Past clipboards are stored in `dired-ranger-copy-ring` so you can
repeat the past pastes.

Call `dired-ranger-paste` or `dired-ranger-move` to copy or move
the files in the current clipboard to the current dired buffer.
With raw prefix argument (usually C-u), the clipboard is not
cleared, so you can repeat the copy operation in another dired
buffer.

### Bookmarks

Use `dired-ranger-bookmark` to bookmark current dired buffer.  You
can later quickly revisit it by calling
`dired-ranger-bookmark-visit`.

A bookmark name is any single character, letter, digit or a symbol.

A special bookmark with name `dired-ranger-bookmark-LRU` represents
the least recently used dired buffer.  Its default value is \`.  If
you bind `dired-ranger-bookmark-visit` to the same keybinding,
hitting \`\` will instantly bring you to the previously used dired
buffer.  This can be used to toggle between two dired buffers in a
very fast way.

These bookmarks are not persistent.  If you want persistent
bookmarks use the bookmarks provided by emacs, see (info "(emacs)
Bookmarks").

## dired-narrow

This package provides live filtering of files in dired buffers.  In
general, after calling the respective narrowing function you type a
filter string into the minibuffer.  After each change the changes
automatically reflect in the buffer.  Typing C-g will cancel the
narrowing and restore the original view, typing RET will exit the live
filtering mode and leave the dired buffer in the narrowed state.  To
bring it back to the original view, you can call `revert-buffer`
(usually bound to <kbd>g</kbd>).

During the filtering process, several special functions are available.
You can customize the binding by changing `dired-narrow-map`.

* `dired-narrow-next-file` (<kbd>\<down\></kbd> or <kbd>C-n</kbd>) - move the point to the next file
* `dired-narrow-previous-file` (<kbd>\<up\></kbd> or <kbd>C-p</kbd>) - move the point to the previous
file
* `dired-narrow-enter-directory` (<kbd>\<right\></kbd> or <kbd>C-j</kbd>) - descend into
the directory under point and immediately go back to narrowing mode

You can customize what happens after exiting the live filtering mode
by customizing `dired-narrow-exit-action`.
`dired-narrow-exit-action` may be executed automatically,
when there is only one file left while narrowing.
In order to enable this feature, add `(setq dired-narrow-exit-when-1-left t)` to your config.
It makes sense when you use find-file as your exit action, e.g.
`(setq dired-narrow-exit-action 'dired-narrow-find-file)`.
A chosen file will be quickly highlighted before executing `dired-narrow-exit-action`.
This behavior is controlled by variables `dired-narrow-enable-blinking`,
`dired-narrow-blink-time` and by a face `dired-narrow-blink`.


These narrowing functions are provided:

* `dired-narrow`
* `dired-narrow-regexp`
* `dired-narrow-fuzzy`

You can also create your own narrowing functions quite easily.  To
define new narrowing function, use `dired-narrow--internal` and
pass it an apropriate filter.  The filter should take one argument
which is the filter string from the minibuffer.  It is then called
at each line that describes a file with point at the beginning of
the file name.  If the filter returns nil, the file is removed from
the view.  As an inspiration, look at the built-in functions
mentioned above.

## dired-list

Produce a file listing with a shell incantation and make a dired
out of it!

This package provides one principal function, `dired-list` which
can be used to produce dired buffers from shell programs outputing
text roughly in the format of `la -ls`.

For most standard output formats the default filter and sentinel
should work, but you can also provide your own if the situation
requires it.

Most of the time you can pipe a zero-delimited list of files to `ls`
through `xargs(1)` using

    | xargs -I '{}' -0 ls -l '{}'

which creates a compatible listing.  For more information read the
documentation of `dired-list`, for example by invoking

    C-h f dired-list RET

in emacs.

In addition to the generic interface this package implements common
listings (patches and extensions welcome!), these are:

* `dired-list-mpc`
* `dired-list-git-ls-files`
* `dired-list-hg-locate`
* `dired-list-locate`
* `dired-list-find-file`
* `dired-list-find-name`
* `dired-list-grep`

## dired-collapse

Often times we find ourselves in a situation where a single file
or directory is nested in a chain of nested directories with no
other content.  This is sometimes due to various mandatory
layouts demanded by packaging tools or tools generating these
deeply-nested "unique" paths to disambiguate architectures or
versions (but we often use only one anyway).  If the user wants
to access these directories they have to quite needlessly
drill-down through varying number of "uninteresting" directories
to get to the content.

This minor mode is in main inspired by how GitHub renders these
paths: if there is a chain of directories where each one only has
one child, they are concatenated together and shown on the first
level in this collapsed form.  When the user clicks this
collapsed directory they are immediately brought to the deepest
directory with some actual content.

To enable or disable this functionality use `dired-collapse-mode` to
toggle it for the current dired buffer.

If the deepest directory contains only a single file this file is
displayed instead of the last directory.  This way we can get
directly to the file itself.  This is often helpful with config
files which are stored in their own directories, for example in
`~/.config/foo/config` and similar situations.

The files or directories re-inserted in this manner will also
have updated permissions, file sizes and modification dates so
they truly correspond to the properties of the file being shown.

The path to the deepest file is dimmed with the `shadow` face so
that it does not distract but at the same time is still available
for inspection.

The mode is integrated with `dired-rainbow` so the nested files
are properly colored according to user's rules.

The directory without collapsed path might look something like this:

![before](http://i.imgur.com/Xje5Nmo.png)

After collapsing:

![after](http://i.imgur.com/mm6gFbX.png)
