;;; dired-hacks.el --- Collection of useful dired additions

;; Copyright (C) 2014-2015 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Keywords: files
;; Version: 0.0.1
;; Created: 14th February 2014
;; Package-requires: ((dash "2.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Collection of useful dired additions.  I don't want this to become
;; another `dired+', so I'm splitting all the functionality into separate
;; mutually independent packages.  All shared functionality and helpers
;; will be extracted into a single package `dired-hacks-utils', so that
;; will be the only dependence.

;; For information about the individual packages, see README.md,
;; or https://github.com/Fuco1/dired-hacks.

;;; Code:

(provide 'dired-hacks)

;;; dired-hacks.el ends here
