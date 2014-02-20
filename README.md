1. [dired-hacks](#)
2. [Contribute!](#contribute!)
3. [Packages](#packages)
    1. [dired-hacks-utils](#dired-hacks-utils)
    2. [dired-filter](#dired-filter)
    3. [dired-avfs](#dired-avfs)
    4. [dired-open](#dired-open)
    5. [dired-rainbow](#dired-rainbow)

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

Adds ibuffer-like filtering to dired.

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
