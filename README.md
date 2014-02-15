# dired-hacks

Collection of useful dired additions.  I don't want this become
another `dired+`, so I'm splitting all the functionality into separate
mutually independent packages.  All shared functionality and helpers
will be extracted into a single package `dired-hacks-utils`, so that
will be the only dependence.

# dired-hacks-utils

Set of utility functions used in all the `dired-hacks` packages.

# dired-filter

Adds ibuffer-like filtering to dired.

# dired-avfs

Adds [avfs](http://avf.sourceforge.net/) support for seamless archive
browsing.  This extension therefore depends on the presence of `avfsd`
on your system.  In debian-derived distributions you can usually do

    apt-get install avfs

`avfs` is probably also available for Mac OS.  You're out of luck on
Windows, sorry.

Once the daemon is installed, run it with `mountavfs` and everything
"Should Just Work™".

# dired-open

While emacs already has the `auto-mode-alist`, this is often
insufficient.  Many times, you want to open media files, pdfs or
other documents with an external application.  There's remedy for
that too, namely `dired-guess-shell-alist-user`, but that is still
not as convenient as just hitting enter.

This package adds a mechanism to add "hooks" to `dired-find-file`
that will run before emacs tries its own mechanisms to open the
file, thus enabling you to launch other application and suspend the
default behaviour.

By default, two additional methods are enabled,
`dired-open-by-extension` and `dired-open-subdir`.

This package also provide two other convenience hooks: first tries
to open the file using `xdg-open`, the other by launching
applications from `dired-guess-shell-alist-user`.  These are not
used by default.

You can customize the list of functions to try by customizing
`dired-open-functions`.

To fall back to the default `dired-find-file`, you can provide the
prefix argument (usually C-u) to the `dired-open-file` function.
This is useful for example when you configure html files to be
opened in browser and you want to edit the file instead of view it.

Note also that this package can handle calls when point is not on a
line representing a file---an example hook is provided to open a
subdirectory under point if point is on the subdir line, see
`dired-open-subdir`.

If you write your own handler, make sure they do *not* throw errors
but instead return nil if they can't proceed.
