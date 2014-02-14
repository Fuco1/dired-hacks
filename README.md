# dired-hacks

Collection of useful dired additions.  I don't want this become another `dired+`, so I'm splitting all the functionality into separate mutually independent packages.  All shared functionality and helpers will be extracted into a single package `dired-hacks-utils`, so that will be the only dependence.

# dired-hacks-utils

Set of utility functions used in all the `dired-hacks` packages.

# dired-filter

Adds ibuffer-like filtering to dired.

# dired-avfs

Adds [avfs](http://avf.sourceforge.net/) support for seamless archive browsing.  This extension therefore depends on the presence of `avfsd` on your system.  In debian-derived distributions you can usually do

    apt-get install avfs

`avfs` is probably also available for Mac OS.  You're out of luck on Windows, sorry.

Once the daemon is installed, run it with `mountavfs` and everything "Should Just Work™".
