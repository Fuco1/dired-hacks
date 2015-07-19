(require 'dired-hacks-utils)
(require 'dash)
(require 'eimp)

(require 'image-mode)

;; TODO: add operations to
;; - rotate
;; - scale
;;   - to original size
;;   - to window size
;;   - +/- given %

;; thumb customizes
(defcustom di-thumbs-directory "~/.emacs.d/di/"
  "Location where thumbnails are stored."
  :type 'directory)

(defcustom di-thumb-relief 2
  ""
  :type 'integer
  )

(defcustom di-thumb-margin 3
  ""
  :type 'integer
  )

(defcustom di-thumb-width 200
  ""
  :type 'integer
  )

(defcustom di-thumb-height 115
  ""
  :type 'integer
  )

(defcustom di-thumb-track t
  "If non-nil, track the selected thumb in corresponding view buffer."
  :type 'boolean)

(defcustom di-header-format "[%c/%t] %f"
  ""
  :type 'string
  )
(put 'di-header-format 'risky-local-variable t)

;; see `format-spec'

(defvar di-thumb-buffer nil
  "Thumb buffer related to this view buffer.")

(defvar di-view-buffer nil
  "View buffer related to this thumb buffer.")

(defvar di-file-list nil
  "List of files displayed in this view buffer.

This is only meaningful if no thumb buffer is associated.")

(defvar di-file-list-current 0
  "Currently viewed item in this view buffer.

This is only meaningful if no thumb buffer is associated.")

(defvar di-active-view-buffer nil
  "Active view buffer.")

(defvar di-active-thumb-buffer nil
  "Active thumb buffer.")


;;; misc
(defmacro di-inc (var modulo &optional delta)
  (setq delta (or delta 1))
  `(setq ,var (mod (+ ,var ,delta) ,modulo)))

(defmacro di-dec (var modulo &optional delta)
  (setq delta (or delta 1))
  `(setq ,var (mod (- ,var ,delta) ,modulo)))


;;; dealing with windows
(defun di--window-width (&optional window)
  "Calculate WINDOW width in pixels.

Defaults to `frame-selected-window'."
  (* (window-width (or window (frame-selected-window)))
     (frame-char-width)))

(defun di--thumbs-per-row (&optional window)
  "Return number of thumbs that will fit on a row in WINDOW.

WINDOW defaults to `frame-selected-window'."
  (/ (di--window-width (or window (frame-selected-window)))
     (+ (* 2 di-thumb-relief)
        (* 2 di-thumb-margin)
        di-thumb-width)))


;;; dealing with file names/paths
(defun di--thumbs-directory ()
  "Return the current thumbnails directory.

Create the thumbnails directory if it does not exist.

This function sanitizes the variable `di-thumbs-directory'."
  (let ((dir (file-name-as-directory
              (expand-file-name di-thumbs-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (message "Creating thumbnails directory..."))
    dir))

;; TODO this should go to /tmp/
(defun di--temp-file (&optional buffer)
  "Return the location of temp file for a view buffer.

Optional argument BUFFER is a view buffer, otherwise default to
the active one."
  (concat (di--thumbs-directory) ".di-temp-" (buffer-name (or buffer di-active-view-buffer))))

;; TODO: add optional dimensions of the thumb
(defun di--thumb-name (file)
  "Return the thumb filename for this FILE."
  (file-truename
   (concat (di--thumbs-directory)
           (replace-regexp-in-string
            (if (eq system-type 'windows-nt) "\\" "/")
            "!" file))))


;;; dealing with images
(defun di--image-spec (file)
  `(image :file ,file
          :type ,(image-type file)))

(defun di--insert-image (file &optional relief margin)
  "Insert image FILE at point.

RELIEF and MARGIN specify the image properties.  See also
`create-image'."
  (let ((i `(image :type ,(image-type file)
                   :file ,file
                   :relief ,(or relief 0)
                   :margin ,(or margin 0))))
    (insert-image i)))

(defun di--resize-image (in-file out-file width height)
  "Resize IN-FILE to WIDTH and HEIGHT, save to OUT-FILE"
  (call-process "convert" nil nil nil "-resize"
                (format "%dx%d" width height)
                in-file out-file))

(defun di--create-fitted-image (file &optional window)
  "Resize FILE to fit current view window."
  (let* ((edges (window-inside-pixel-edges window))
         (width (- (nth 2 edges) (nth 0 edges)))
         (height (- (nth 3 edges) (nth 1 edges))))
    (di--resize-image file (di--temp-file) width height)))

(defun di--open-image (file &optional window)
  "Open FILE as image in current buffer."
  ;; TODO: we need to be careful in what window we are
  (let ((inhibit-read-only t))
    (di--create-fitted-image file window)
    (erase-buffer)
    (clear-image-cache)
    (di--insert-image (di--temp-file))
    (image-mode-setup-winprops)))


;;; dealing with thumbnails
(defun di--thumb-at-point-p (&optional point)
  "Return true if there is a thumb at point.

If POINT is specified, test this point instead."
  (get-text-property (or point (point)) 'di-thumbnail))

(defun di--create-thumb (file thumb)
  "Create a thumb of FILE and save as THUMB."
  (call-process "convert" nil nil nil "-thumbnail"
                (format "%dx%d" di-thumb-width di-thumb-height)
                file thumb))

(defun di--generate-thumbs (files)
  "Generate thumbnails for FILES.

FILES can be a single string describing a file path or a list of
strings describing file paths."
  (--each (if (listp files) files (list files))
    (let ((thumb-name (di--thumb-name it)))
      (if (and (not (file-exists-p thumb-name))
               (not (= 0 (di--create-thumb it thumb-name))))
          (message "Thumb could not be created for file %s" it)))))

(defun di--arrange-thumbs (&optional window)
  "Arrange thumbs in this view buffer in rows.

The number of thumbs per row is calculated using
`di--thumbs-per-row'."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (row (di--thumbs-per-row (or window (frame-selected-window))))
          (count 0))
      (save-excursion (replace-string "\n" ""))
      (while (not (eobp))
        (forward-char row)
        (unless (eobp) (insert "\n"))))))

(defun di--insert-thumb (file dired-buffer)
  "Insert thumbnail image FILE.

DIRED-BUFFER is an associated dired buffer."
  (let* ((thumb-name (di--thumb-name file))
         (beg (point))
         (size (image-size `(image :file ,thumb-name
                                   :type ,(image-type thumb-name)) t)))
    (di--insert-image thumb-name
                      di-thumb-relief
                      (cons (+ di-thumb-margin (/ (- di-thumb-width (car size)) 2))
                            di-thumb-margin))
    (add-text-properties
     beg (point)
     (list :di-thumbnail t
           :original-file-name file
           :dired-buffer dired-buffer
           :mouse-face 'highlight))))

(defun di--insert-thumbs (files dired-buffer)
  "Insert thumbnails of FILES.

DIRED-BUFFER is an associated dired buffer."
  (--each files (di--insert-thumb it dired-buffer)))


;;; interactive/dired
(defun di-display-thumbs (arg)
  "Display thumbnails of marked files.

With prefix argument \\[universal-argument] append to the active thumb buffer instead of
replacing it.

With prefix argument \\[universal-argument] \\[universal-argument] open a new thumb buffer,
 prompting for name."
  (interactive "P")
  ;; TODO: add dired+ `dired-get-marked-files' support
  (let ((marked-files (dired-get-marked-files))
        (buf (if (not (equal arg '(16)))
                 (di--get-active-thumb-buffer)
               (let* ((default (concat "*di"
                                       (symbol-name (gensym "-thumb-"))
                                       "*"))
                      (name (read-string (format "Thumb buffer name [default %s]: " default)
                                         nil
                                         nil
                                         default)))
                 (di--spawn-thumb-buffer name))))
        (dir-buf (current-buffer))
        (inhibit-read-only t))
    (di--generate-thumbs marked-files)
    (pop-to-buffer buf)
    (with-current-buffer buf
      (unless (equal arg '(4))
        (erase-buffer))
      (when (equal arg '(4))
        (goto-char (point-max)))
      (save-excursion
        (di--insert-thumbs marked-files dir-buf)
        (--when-let (di--get-active-thumb-windows)
          (di--arrange-thumbs (car it)))))))

;; NOTE: (dired-get-marked-files) automagically returns the file under
;; cursor if no selection is made.
;; TODO: cleanup: remove the temp files for views which no longer exists
(defun di-view-files ()
  "View marked files."
  (interactive)
  (let ((marked-files (dired-get-marked-files))
        (buf (di--spawn-view-buffer)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (di--open-image (car marked-files))
      (set (make-local-variable 'di-file-list) marked-files)
      (set (make-local-variable 'di-file-list-current) 0))))


;;; view mode
(defvar di-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map (kbd ".") 'di-view-next)
    (define-key map (kbd ",") 'di-view-previous)
    (define-key map (kbd "s s") 'di-view-fit-image-to-window)
    (define-key map (kbd "<right>") 'di-view-next)
    (define-key map (kbd "<left>") 'di-view-previous)
    map))

(defun di--spawn-view-buffer (&optional name)
  "Spawn a new view buffer and set it as active."
  (let ((buf (get-buffer-create (or name
                                    (concat
                                     "*di"
                                     (symbol-name (gensym "-view-"))
                                     "*")))))
    (setq di-active-view-buffer buf)
    (with-current-buffer di-active-view-buffer
      (di-view-mode)
      (add-hook 'kill-buffer-hook `(lambda () (delete-file ,(di--temp-file (current-buffer)))) nil 'local))
    buf))

(defun di--get-active-view-buffer ()
  "Return the active view buffer."
  (unless (and di-active-view-buffer
               (buffer-live-p di-active-view-buffer))
    (di--spawn-view-buffer "*di-view*"))
  di-active-view-buffer)

(defun di--get-active-view-windows ()
  "Return a list of windows displaying active view buffer."
  (let ((active (di--get-active-view-buffer)))
    (--filter (equal (window-buffer it)
                     active) (-mapcat 'window-list (frame-list)))))

(defun di--view-current-index ()
  "Return the index of currently viewed image in this view buffer."
  (if di-thumb-buffer
      (with-current-buffer di-thumb-buffer
        (di--thumb-current-index))
    di-file-list-current))

(defun di--view-current-file ()
  "Return the filename of currently viewed image in this view buffer."
  (if di-thumb-buffer
      (with-current-buffer di-thumb-buffer
        (plist-get (text-properties-at (point)) :original-file-name))
    (nth di-file-list-current di-file-list)))

(defun di--view-total ()
  "Return total amount of images are associated with this view buffer."
  (if di-thumb-buffer
      (with-current-buffer di-thumb-buffer
        (di--thumb-total))
    (length di-file-list)))

(defun di-view-next (&optional arg)
  "Display next file"
  (interactive "p")
  (if di-thumb-buffer
      (with-current-buffer di-thumb-buffer
        (di-thumb-next)
        ;; TODO: this should set point in all the windows
        (set-window-point (car (di--get-active-thumb-windows)) (point)))
    (redisplay)
    (di-inc di-file-list-current (di--view-total) arg)
    (di--open-image (nth di-file-list-current di-file-list))))

(defun di-view-previous (&optional arg)
  "Display previous file."
  (interactive "p")
  (if di-thumb-buffer
      (with-current-buffer di-thumb-buffer
        (di-thumb-previous)
        (set-window-point (car (di--get-active-thumb-windows)) (point)))
    (di-dec di-file-list-current (di--view-total) arg)
    (di--open-image (nth di-file-list-current di-file-list))))

(defun di-view-fit-image-to-window ()
  "Fit image to window."
  (interactive)
  (di--open-image (di--view-current-file)))

(define-derived-mode di-view-mode special-mode
  "DI View"
  "docs"
  (use-local-map di-view-mode-map)
  (setq cursor-type nil truncate-lines t
        image-type (plist-get (cdr (image-get-display-property)) :type))
  (setq header-line-format
        '(:eval (format-spec
                 di-header-format
                 (list
                  (cons ?c (1+ (di--view-current-index)))
                  (cons ?t (di--view-total))
                  (cons ?F (di--view-current-file))
                  (cons ?f (file-name-nondirectory (di--view-current-file))))))))


;;; thumb mode
(defvar di-thumb-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "f") 'di-thumb-next)
    (define-key map (kbd "b") 'di-thumb-previous)
    (define-key map (kbd "n") 'di-thumb-next-line)
    (define-key map (kbd "p") 'di-thumb-previous-line)
    (define-key map (kbd ".") 'di-thumb-next)
    (define-key map (kbd ",") 'di-thumb-previous)
    (define-key map (kbd "<right>") 'di-thumb-next)
    (define-key map (kbd "<left>") 'di-thumb-previous)
    (define-key map (kbd "<down>") 'di-thumb-next-line)
    (define-key map (kbd "<up>") 'di-thumb-previous-line)
    (define-key map (kbd "RET") 'di-thumb-preview)
    map))

(defun di--spawn-thumb-buffer (name)
  "Spawn a new thumb buffer and set it as active."
  (let ((buf (get-buffer-create name)))
    (setq di-active-thumb-buffer buf)
    (with-current-buffer di-active-thumb-buffer
      (di-thumb-mode))
    buf))

(defun di--get-active-thumb-buffer ()
  "Return the active thumb buffer."
  (unless (and di-active-thumb-buffer
               (buffer-live-p di-active-thumb-buffer))
    (di--spawn-thumb-buffer "*di-thumb*"))
  di-active-thumb-buffer)

(defun di--get-active-thumb-windows ()
  "Return a list of windows displaying active view buffer."
  (let ((active (di--get-active-thumb-buffer)))
    (--filter (equal (window-buffer it)
                     active) (-mapcat 'window-list (frame-list)))))

(defun di--thumb-current-index ()
  "Return index of the currently highlighted thumb."
  (+ (* (1- (line-number-at-pos))
        (di--thumbs-per-row (car (di--get-active-thumb-windows))))
     (current-column)))

(defun di--thumb-total ()
  "Return number of thumbs displayed in this thumb buffer."
  (length
   (replace-regexp-in-string
    "\n" ""
    (buffer-substring-no-properties (point-min) (point-max)))))

(defun di-thumb-next ()
  (interactive)
  (forward-char)
  (when (and (looking-at "$") (not (eobp)))
      (forward-char))
  (when (eobp)
    (goto-char (point-min)))
  (when (and di-view-buffer
             di-thumb-track)
    (di-thumb-preview t)))

(defun di-thumb-previous ()
  (interactive)
  (if (bobp)
      (progn
        (goto-char (point-max))
        (backward-char))
    (backward-char)
    (when (looking-at "$")
      (backward-char)))
  (when (and di-view-buffer
             di-thumb-track)
    (di-thumb-preview t)))

(defun di-thumb-next-line ()
  (interactive)
  (let ((cc (current-column)))
    (forward-line)
    (condition-case er
        (forward-char cc)
      (end-of-buffer
       (goto-char (point-min))
       (forward-char cc)))
    (when (eobp)
      (goto-char (point-min))
      (forward-char cc)))
  (when (and di-view-buffer
             di-thumb-track)
    (di-thumb-preview t)))

(defun di-thumb-previous-line ()
  (interactive)
  (let ((cc (current-column)))
    (if (= (forward-line -1) -1)
        (progn
          (goto-char (point-max))
          (beginning-of-line)
          (condition-case er
              (progn
                (forward-char cc)
                (when (eobp)
                  (forward-line -1)
                  (forward-char cc)))
            (end-of-buffer
             (forward-line -1)
             (forward-char cc))))
      (forward-char cc)))
  (when (and di-view-buffer
             di-thumb-track)
    (di-thumb-preview t)))

(defun di-thumb-preview (&optional arg)
  "Open the thumb at point in view buffer.

With ARG, do not pop to the view window."
  (interactive "P")
  (let ((buf (if (and di-view-buffer
                      (buffer-live-p di-view-buffer))
                 di-view-buffer
               (di--spawn-view-buffer)))
        (thumb-buf (current-buffer))
        ;; TODO: abstract this into a data structure about thumbs/views
        (tps (text-properties-at (point)))
        (inhibit-read-only t))
    (setq di-active-view-buffer buf)
    (setq di-active-thumb-buffer thumb-buf)
    (set (make-local-variable 'di-view-buffer) buf)
    ;; TODO: only call this if there is no window displaying buf
    (if arg
        (display-buffer buf)
      (pop-to-buffer buf))
    (with-current-buffer buf
      (di--open-image (plist-get tps :original-file-name)
                      (car (di--get-active-view-windows)))
      (set (make-local-variable 'di-thumb-buffer) thumb-buf))))

(define-derived-mode di-thumb-mode special-mode
  "DI Thumb"
  "docs"
  (use-local-map di-thumb-mode-map))

(provide 'dired-images)
