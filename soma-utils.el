;;; soma-utils.el --- Utility functions used throughout the configuration  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (s "0") (evil "0"))

;; For cl-case, cl-assert, cl-first, cl-remove-if and friends:
(require 'cl-lib)

;; evil is a declared dependency (see Package-Requires above).  It is required
;; at run time as well as compile time: evil-with-single-undo is a macro that
;; must be known when this file is byte-compiled, and its expansion and several
;; other functions in this file call evil functions at run time:
(require 'evil)

;; Built-in libraries used below: string-remove-prefix (a defsubst) and
;; url-type (a cl-defstruct accessor):
(require 'subr-x)
(require 'url-parse)

;; Functions defined in other packages or in init files.  Declared so the
;; byte-compiler knows they exist; they are not loaded here.
(declare-function calc-enter-result "calc")
(declare-function math-read-expr "calc-ext")
(declare-function ivy-read "ivy")
(declare-function counsel--find-file-matcher "counsel")
(declare-function org-table-convert-region "org-table")
(declare-function org-table-hline-and-move "org-table")
(declare-function get-clipboard-contents "soma-get-clipboard-contents-init")
(declare-function xclip-set-selection "xclip")
(declare-function pbcopy-set-selection "soma-pbcopy-init")

;; Defined by recentf; soma-purge-recentf reads and assigns it:
(defvar recentf-list)

;;------------------------------------------------------------------------
;;
;; * Delete trailing whitespace except on the current line
;;
;; This function is necessary for yasnippet, which tends to leave a
;; bunch of trailing whitespace around when a snippet exits.  However,
;; although we want to delete most trailing whitespace, we want to
;; leave it on the current line because that's where the point will
;; insert according to the snippet definition.
;;
(defun soma-delete-trailing-whitespace-except-on-current-line ()
  (interactive)
  (save-excursion
    ;; Delete trailing whitespace on all lines after the current line,
    ;; if there are any.  This goes first so that the line positions
    ;; used below are not shifted.
    (when (< (line-end-position) (point-max))
      (delete-trailing-whitespace (1+ (line-end-position)) (point-max)))
    ;; Delete trailing whitespace on all lines before the current line,
    ;; if there are any:
    (when (> (line-beginning-position) (point-min))
      (delete-trailing-whitespace (point-min) (line-beginning-position)))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; From:
;;
;;   https://www.reddit.com/r/emacs/comments/hv3kzf/weekly_tipstricketc_thread/fywkxej/
;;
;; "I found calc's 16# hex number notation annoying, so I made a
;;  function to grab a hex number at point into calc,
;;  e.g. 0x7ffff74cdaed:
;;
;;  e.g. Yank 0x4526a into calc without having to change it to 16#4526a
;;

(defun soma--calc-yank-hex ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (hex-num (buffer-substring-no-properties (car bounds)
                                                  (cdr bounds))))
    (setq hex-num (concat "16#" (string-remove-prefix "0x" hex-num)))
    (calc-enter-result 0 "grab" (math-read-expr hex-num))))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

;; counsel.el defines this variable, and soma--choose-directory binds it, but
;; counsel is not loaded when this file is byte-compiled.  Declare it special
;; here so that the let binding below stays dynamic in the compiled code and
;; counsel--find-file-matcher can see it:
(defvar counsel--find-file-predicate)

(defun soma--choose-directory (directory-to-start-in)
  "Return a directory chosen by the user.  THe user will be prompted
to choose a directory starting with `directory-to-start-in'"
  (let* ((ivy-read-prompt "Choose directory: ")
         (counsel--find-file-predicate #'file-directory-p)
         (default-directory directory-to-start-in)
         (selected-directory
          (ivy-read
           ivy-read-prompt
           #'read-file-name-internal
           :matcher #'counsel--find-file-matcher)))
    selected-directory))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--existing-or-parent-directory (file-or-directory)
  "If `file-or-directory' exists, return it.

Otherwise, returns its parent directory."
  (pcase file-or-directory
    ('nil nil)
    ((pred file-exists-p) file-or-directory)
    ((guard (stringp file-or-directory))
     ;; soma--parent-directory returns nil when there is no parent, so
     ;; stop (and return nil) rather than calling file-exists-p on nil:
     (cl-loop
      while (and file-or-directory
                 (not (file-exists-p file-or-directory)))
      do (setq file-or-directory
               (soma--parent-directory file-or-directory))
      finally return file-or-directory))))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--escape-string (s)
  "Escape % in string, by quadrupling the %.
Not sure why simply doubling it doesn't work, but merely doubling
it leads a \"Not enough arguments for format string\" error"
  (cl-assert (stringp s))
  ;; Not sure why escaping with just %% doesn't work
  (replace-regexp-in-string "%" "%%%%" s))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--escape-and-truncate (s)
  "Escape % in string s and truncate it"
  (cl-assert (stringp s))
  (let ((escaped-s
         (soma--escape-string s)))
    (soma--truncate-with-elipsis-if-over-length escaped-s 72)))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--maybe-format-url (maybe-url)
  "If `maybe-url' is a URL, escape % within it and truncate it,
if it's not then return an error message colored in red"
  ;; validate URL
  (if (soma--url-p maybe-url)
      ;; this looks like a valid URL
      ;;
      ;; we need to escape it so that we don't get a
      ;; "Not enough arguments for format string" error
      (soma--escape-and-truncate maybe-url)
    ;; this is not a valid URL
    (format "[[%s]]"
            (propertize "NOT A URL" 'face '(:foreground "red")))))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--parent-directory (file-or-directory)
  "Returns the parent directory of the given `file-or-directory' string"
  (cl-assert (stringp file-or-directory))
  (let ((stripped-last-component
         (directory-file-name file-or-directory)))
    (if stripped-last-component
        (let ((parent-dir-with-trailing-slash
               (file-name-directory stripped-last-component)))
          (if parent-dir-with-trailing-slash
              (directory-file-name parent-dir-with-trailing-slash)
            nil))
      nil)))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--truncate-with-elipsis-if-over-length (s limit)
  "Truncate given string `s' if its length is over the `limit',
then add elipsis"
  (if (> (length s) limit)
      (concat
       (substring-no-properties s 0 limit)
       "...")
    s))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--truncate-if-over-length (s limit)
  "Truncate given string `s' if its length is over the `limit'"
  (if (> (length s) limit)
      (substring-no-properties s 0 limit)
    s))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--git-url-p (maybe-git-url)
  "Returns `t' if `maybe-url' looks like a git URL, `nil' otherwise"
  (if (soma--url-p maybe-git-url)
      ;; valid URL
      (if (string-match
           (rx-to-string
            '(seq
              ".git"
              eol))
           maybe-git-url)
          t
        ;; not a git URL
        nil)
    ;; not a URL
    nil))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--url-p (maybe-url)
  "Returns `t' if `maybe-url' looks like a URL, `nil' otherwise"
  (if (url-type (url-generic-parse-url maybe-url))
      ;; valid URL
      t
    ;; not a URL
    nil))

;;
;;------------------------------------------------------------------------


;;------------------------------------------------------------------------
;;

(defun soma--get-from-x-or-kill-ring (from)
  "Returns contents of X primary selection or the top
of the emacs kill ring, depending on whether `from'
is `emacs' or `x'"
  (let ((result
         (cl-case from
           (emacs
            (soma-utils-chomp
             (substring-no-properties (car kill-ring))))
           (x
            (soma-utils-chomp
             (get-clipboard-contents)))
           (otherwise
            (error (concat "soma--get-from-x-or-kill-ring: Error: "
                           "Invalid argument %s\n"
                           "Valid arguments are 'emacs or 'x")
                   from)))))
    result))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; The functions below are used to insert numbers before
;; the region rectangle.

(defun soma-format-guesser (start end start-at)
  "Return a guess at a suitable format string.

START should be the position on the first line
of interest, END should be the position on the last
of interest, and START-AT should be the number to
start at."
  (let* ((first-line (line-number-at-pos start))
         (last-line (line-number-at-pos end))
         (total-lines (- last-line first-line))
         (ending-number (+ start-at total-lines))
         (number-of-digits-in-ending-number
          (length (number-to-string ending-number)))
         (first-format-str-guess
          (concat
           "%0"
           (number-to-string number-of-digits-in-ending-number)
           "d"))
         (second-format-str-guess
          (concat
           "%"
           (number-to-string number-of-digits-in-ending-number)
           "d")))
    (list first-format-str-guess second-format-str-guess)))

(defun soma--get-stringified-region ()
  "Returns the current region as a string read by elisp's string reader.
Note: The region to be read must include an outer set of double quotes."
  (interactive)
  (let* ((region
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
         (stringified-region
          (car (read-from-string region))))
    (if (equal
         (type-of stringified-region)
         'string)
        stringified-region
      (error
       (concat
        "soma--get-stringified-region: "
        "Error: region must include an outer set of double quotes")))))


(defun soma--get-all-open-buffer-names ()
  "Return a list of all open buffer names"
  (let* ((buffers (buffer-list))
         (buffer-names (mapcar (lambda (buffer)
                              (buffer-name buffer)) buffers)))
    buffer-names))

(defun soma--left-most-non-whitespace-column-in-region (beginning end)
  "Return the left-most non-whitespace column in region"
  (interactive "r")
  (cl-assert (integerp beginning))
  (cl-assert (integerp end))
  (cl-assert (>= beginning 0))
  (cl-assert (<= end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region beginning end)
      (goto-char (point-min))
      (let ((left-most-non-whitespace-column
             (cl-loop
              with found = 10000000000
              until (or
                     (eobp)
                     (not
                      (re-search-forward
                       (rx-to-string
                        `(seq
                          (not
                           (any whitespace "\n"))))
                       nil
                       t)))
              ;; Note: we don't look at columns here, because
              ;;       org-mode can lie about them within links
              do (setq found (- (point)
                                (line-beginning-position)
                                1))
              minimize found)))
        left-most-non-whitespace-column))))

;; The following function is much like rectangle-number-lines,
;; which it calls, but it also call a function to guess
;; the format string, and it's done it will position point
;; at the first number it inserted.
(defun soma-rectangle-number-lines-alt(start end start-at &optional format _format-guesser)
  "Insert numbers in front of the region-rectangle.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to ‘format’ along with the line count.  When called interactively
with a prefix argument, prompt for START-AT and FORMAT.

When non-nil, FORMAT-GUESSER should be a function which
accepts START END START-AT as arguments and returns a list
of format strings suitable for passing to the `format' function.

When rectangle-number-lines-alt is called interactively it will
prompt the user with the first format string returned by FORMAT-GUESSER,
and the rest of the format strings will be offered as possible completions."
  (interactive
    (let* ((mark (mark))
           (point (point))
           (start (min point mark))
           (end (max point mark))
           (start-at (cl-parse-integer (ivy-read "Start at: " nil)))
           (format-str-guesses (soma-format-guesser start end start-at))
           (ivy-read-prompt "Format: ")
           (ivy-read-collection format-str-guesses)
           (ivy-read-initial-input (car format-str-guesses))
           (format (ivy-read
                    ivy-read-prompt
                    ivy-read-collection
                    :initial-input ivy-read-initial-input)))
      (list start end start-at format nil)))
  (cl-assert (< start end))
  (let* ((first-line (line-number-at-pos start))
         (last-line (line-number-at-pos end))
         (total-lines (- last-line first-line))
         (ending-number (if start-at
                            (+ start-at total-lines)
                          (+ 1 total-lines)))
         (number-of-digits-in-ending-number
          (length (number-to-string ending-number))))
    (rectangle-number-lines start end start-at format)
    (let* ((new-start
            (+ start number-of-digits-in-ending-number))
           (new-end
            (point)))
      (evil-visual-refresh new-start new-end (evil-visual-type))
      (evil-exit-visual-state)
      (goto-char start))))

;;
;;------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-utils-chomp (given-string)
  "Return a copy of the GIVEN-STRING without any trailing
whitespace or newlines it may have."
  (cl-assert (stringp given-string))
  (let ((trailing-newlines
         (rx (:
               (one-or-more
                (any whitespace "\n"))
               string-end))))
        (replace-regexp-in-string
         trailing-newlines ; Search for this
         ""                ; Replace with this
         given-string)))   ; Search inside this
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-utils-get-left-most-col-with-non-whitespace-char-in-region (beg end)
  "Return the number of the left-most column with a non-whitespace character
in a region."
  ; We need to exit the visual state first, otherwise save-excursion
  ; will move point to (region-beginning) after it's done, becuase
  ; while a region is active (point) for some unknown reason evaluates
  ; to (region-beginning)
  (cl-assert (integerp beg))
  (cl-assert (integerp end))
  (cl-assert (>= beg 0))
  (cl-assert (<= end (point-max)))
  (evil-exit-visual-state)
  (let ((left-most-col-with-non-whitespace-char 10000000000000))
    (save-excursion
      (let ((start-of-line-at-region-beg
             (progn
               (goto-char beg)
               (forward-line 0)
               (point)))
            (end-of-line-at-region-end
             (progn
               (goto-char end)
               (end-of-line)
               (point))))
        ; We pass narrow-to-region the start and end of line
        ; at region beginning and end because for some unknown
        ; reason (region-beginning) and (region-end) return
        ; the wrong values when used in visual state from a hydra.
        ; That may be a hydra bug.
        ;
        ; As a workaround, for now we'll narrow to the beginning
        ; of the line at the (region-beginning) position and end
        ; of the line at the (region-end) position, which seems
        ; to work.
        (narrow-to-region
         start-of-line-at-region-beg
         end-of-line-at-region-end)
        (goto-char beg)
        (while (not (equal (point) (point-max)))
          (forward-line 0)
          (let ((chars-moved (skip-syntax-forward " ")))
            (when (< chars-moved left-most-col-with-non-whitespace-char)
              (setq left-most-col-with-non-whitespace-char chars-moved)))
          (forward-line 1)))
      (widen)
      (evil-visual-make-region beg end))
    left-most-col-with-non-whitespace-char))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------
;;
(defun soma--utils-get-mimetype (file)
  "Get mimetype for given `file', which is a path given as a string"
  (cl-assert (stringp file))
  (soma-utils-chomp
   (shell-command-to-string
    (format "file --brief --mime-type %s" (shell-quote-argument file)))))
;;
;;------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Thanks to jlf from #emacs
;;
(defun soma-utils-uniq (given-list)
  "Return a list of the unique elements from a given list"
  (cl-assert (listp given-list))
  (let (new-list)
    (mapc
     (lambda (element)
       (add-to-list 'new-list element))
     given-list)
    new-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-utils-parent-filename (filename n)
  "Return FILENAME with N dirs removed from the end.

For example,
  (soma-utils-parent-filename \"foo/bar/baz\" 1)
  \"foo/bar/\"

  (soma-utils-parent-filename \"foo/bar/baz\" 2)
  \"foo/\"

  (soma-utils-parent-filename \"foo/bar/baz\" 3)
  \"\"

  (soma-utils-parent-filename \"foo/bar/baz\" 4)
  \"\""
  (cl-assert (stringp filename))
  (cl-assert (integerp n))
  (cl-labels ((soma-utils-parent-filename-aux (filename n)
             (if (> n 0)
                 (let* ((trimmed-filename (replace-regexp-in-string
                                           "/$"
                                           ""
                                           filename))
                        (match (string-match
                                (rx
                                 (:
                                  (one-or-more anything)
                                  "/"))
                                trimmed-filename))
                        (parent-filename
                         (if match
                             (substring trimmed-filename
                                        match
                                        (match-end 0))
                           "")))
                   (soma-utils-parent-filename-aux
                    parent-filename
                    (- n 1)))
               filename)))
    (let* ((trimmed-filename (replace-regexp-in-string
                              "/$"
                              ""
                              filename))
           (number-of-slashes
            (- (length
                (split-string trimmed-filename "/"))
               1))
           (n (cond ; Make sure n is within limits
                ((> n number-of-slashes) (+ number-of-slashes 1))
                ((< n 0) 0)
                (t n))))
      (soma-utils-parent-filename-aux filename n))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-utils-pp-region (start end)
  "Replace the selected region with a pretty-printed copy of it. Non-interactive."
  (cl-assert (integerp start))
  (cl-assert (integerp end))
  (cl-assert (>= start 0))
  (cl-assert (<= end (point-max)))
  (let ((pretty-printed
         (let ((text (buffer-substring start end)))
           ;; with-temp-buffer kills the buffer even if pp-buffer signals.
           (with-temp-buffer
             (insert text)
             (pp-buffer)
             (buffer-string)))))
    (delete-region start end)
    (save-excursion
      (goto-char start)
      (insert pretty-printed))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-utils-pp-region-interactive (start end)
  "Replace the selected region with a pretty-printed copy of it"
  (interactive "r")
  (cl-assert (integerp start))
  (cl-assert (integerp end))
  (cl-assert (>= start 0))
  (cl-assert (<= end (point-max)))
  (soma-utils-pp-region start end))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun soma-get-all-buffer-modes-in-frame ()
  (let* ((original-window (selected-window))
         (original-buffer (current-buffer)))
    (save-excursion
      (let ((major-modes
             (mapcar
              (lambda
                (window)
                (progn
                  (select-window window)
                  major-mode))
              (window-list))))
        (select-window original-window)
        (switch-to-buffer original-buffer)
        major-modes))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------
;;
;; * Check to see if point is on a blank line
;;
(defun soma-point-on-empty-line? ()
  (equal
   (line-end-position)
   (line-beginning-position)))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Paste from X or OSX clipboard
;;
;; Thanks to pkkm on #emacs
;;
(defun soma-paste-from-clipboard ()
  "Use paste the contents of the X or OSX clipboard."
  (interactive)
  (let ((clipboard-contents (get-clipboard-contents)))
    (when clipboard-contents
      (save-excursion
        ;; The ignore-errors below is necessary to keep emacs from aborting this
        ;; function when the point is on an empty line.
        (ignore-errors
          ;; Make sure to only move forward a character if point not on an empty line.
          (if (soma-point-on-empty-line?)
              ;; Point is on an empty line.
              (insert clipboard-contents)
              ;; Point is not on an empty line.
              (progn
                (forward-char)
                (insert clipboard-contents))))))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--yank-region-to-clipboard ()
  "Yank region to X, Cygwin, or OSX clipboard"
  (interactive)
  (pcase system-type
    ('gnu/linux (soma-yank-region-to-x-clipboard))
    ('darwin (soma-yank-to-osx-clipboard))
    ('cygwin (soma--yank-region-to-windows-clipboard))
    (_ (error (concat
               "soma--yank-region-to-clipboard: "
               "Error: Unknown system type '%s'")
              system-type))))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--yank-region-to-windows-clipboard ()
  "Yank region to Windows clipboard"
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (save-excursion
          (write-region (region-beginning) (region-end) "/dev/clipboard")
          (message "Yanked region to clipboard!"))
        (goto-char (mark)) ;; Restore point to where visual selection began.
        (evil-exit-visual-state))
    (message "No region active; can't yank to clipboard!")))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
(defun soma-yank-region-to-x-clipboard ()
  "Yank region to X clipboard

   http://www.emacswiki.org/emacs/CopyAndPaste"
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (save-excursion
          ;; If run from a gui emacs, xsel and xclip both hang emacs,
          ;; so we want to use the native emacs clipboard sending feature instead.
          (if (display-graphic-p)
              ;; We are running in a gui emacs.
              ;; Temporarily enable select-enable-clipboard so that the native
              ;; emacs clipboard sending feature is used.  The let binding
              ;; restores the old value even if evil-yank signals an error.
              (let ((select-enable-clipboard t))
                (evil-yank evil-visual-beginning
                           evil-visual-end
                           (evil-visual-type)))
            ;; We are *not* running in a gui emacs.
            (progn
              (shell-command-on-region (region-beginning) (region-end)
                                       "xsel -i -b")
              (message "Yanked region to clipboard!")))
          (message "Yanked region to clipboard!"))
        (goto-char (mark)) ;; Restore point to where visual selection began.
        (evil-exit-visual-state))
    (message "No region active; can't yank to clipboard!")))
;;;(define-key evil-visual-state-map (kbd "RET") 'soma-yank-to-x-clipboard)
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Yank to OSX clipboard
;;
(defun soma-yank-to-osx-clipboard ()
  (interactive)
  (if (evil-visual-state-p)
      (progn
        (save-excursion
          (progn
            (shell-command-on-region (region-beginning) (region-end)
                                     "pbcopy")
            (message "Yanked region to clipboard!")))
        (goto-char (mark)) ;; Restore point to where visual selection began.
        (evil-exit-visual-state))
    (message "No region active; can't yank to clipboard!")))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * soma-find-variable-name
;;
;; Find the names of variables holding a value
;;
;;    (Thanks to turbofail from #emacs)
;;
(defun soma-find-variable-name (val)
  "Find the names of variables holding a value."
  (let (out)
    (mapatoms
     (lambda (sym)
       (when (and (boundp sym) (eq (symbol-value sym) val))
         (push sym out))))
    out))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * soma-current-local-map-name
;;
;; Print the name of the current-local-map.
;;
(defun soma-current-local-map-name ()
  "Print the name of the current-local-map."
  (interactive)
  (let ((map (current-local-map)))
    ;; Without a local map, (current-local-map) is nil, and
    ;; soma-find-variable-name would return every variable whose value is nil.
    (if map
        (print (cl-first (soma-find-variable-name map)))
      (message "No local map in this buffer"))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * cleanup-buffer
;;
;; https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el#L144-166
;;
(defun soma-untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun soma-indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun soma-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (delete-trailing-whitespace))

(defun soma-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (soma-untabify-buffer)
  (soma-cleanup-buffer-safe)
  (soma-indent-buffer))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Remove undesirable filenames from the recentf list
;;
(defun soma-purge-recentf ()
  (let* ((forbidden-files
          (rx (or
                (: string-start "/tmp/clipedit")
              )))
         (filtered-list
           (cl-remove-if
             #'(lambda (name)
                 (string-match forbidden-files name))
             recentf-list)))
    (setq recentf-list filtered-list)))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * ZZ will save and kill the current buffer,
;;   instead of saving and quitting emacs
;;
;; (thanks to cofi on freenode's #evil-mode
;;
(defun soma-save-and-kill-buffer ()
  (interactive)
  (save-buffer)
  (soma-purge-recentf)
  (delete-frame))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * next-error-follow-minor-mode
;;
;;   A mode where moving point visits each file under point.
;;
;; We want to make a function to toggle this mode on and off.
;;
(defun soma-toggle-next-error-follow-mode ()
  (interactive)
  (let ((enable   1)
        (disable -1))
    (if next-error-follow-minor-mode
        (next-error-follow-minor-mode disable)
      (next-error-follow-minor-mode   enable))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Yank the current directory to the clipboard
;;
(defun soma-yank-current-directory-to-clipboard ()
  (interactive)
  (progn
    ;; If run from a gui emacs, xsel and xclip both hang emacs,
    ;; so we want to use the native emacs clipboard sending feature instead.
    (if (display-graphic-p)
        ;; We are running in a gui emacs.
        ;; Temporarily enable select-enable-clipboard so that the native emacs
        ;; clipboard sending feature is used.  The let binding restores the
        ;; old value even if kill-new signals an error.
        (let ((select-enable-clipboard t))
          (kill-new default-directory))
      ;; We are *not* running in a gui emacs.
      (pcase system-type
        ('gnu/linux (xclip-set-selection 'primary default-directory))
        ('darwin (pbcopy-set-selection default-directory))
        (_ (error (concat
                   "soma-yank-current-directory-to-clipboard: "
                   "Error: Unknown system type '%s'")
                  system-type)))
    (message "Yanked the current directory to the clipboard!"))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Yank the current directory to the X clipboard
;;
(defun soma-yank-current-directory-to-x-clipboard ()
  (interactive)
  (progn
    ;; If run from a gui emacs, xsel and xclip both hang emacs,
    ;; so we want to use the native emacs clipboard sending feature instead.
    (if (display-graphic-p)
        ;; We are running in a gui emacs.
        ;; Temporarily enable select-enable-clipboard so that the native emacs
        ;; clipboard sending feature is used.  The let binding restores the
        ;; old value even if kill-new signals an error.
        (let ((select-enable-clipboard t))
          (kill-new default-directory))
      ;; We are *not* running in a gui emacs.
      (xclip-set-selection 'primary default-directory))
    (message "Yanked the current directory to the X clipboard!")))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * Yank the face at point to the X clipboard
;;
(defun soma-yank-face-at-point-to-x-clipboard ()
  "Yank the face at point to the X clipboard"
  (interactive)
  (let ((face-at-point (format "%s" (get-text-property (point) 'face))))
    ;; If run from a gui emacs, xsel and xclip both hang emacs,
    ;; so we want to use the native emacs clipboard sending feature instead.
    (if (display-graphic-p)
        ;; We are running in a gui emacs.
        ;; Temporarily enable select-enable-clipboard so that the native emacs
        ;; clipboard sending feature is used.  The let binding restores the
        ;; old value even if kill-new signals an error.
        (let ((select-enable-clipboard t))
          (kill-new face-at-point))
      ;; We are *not* running in a gui emacs.
      (xclip-set-selection 'primary face-at-point))
    (message
     (format "Face at point: %s" face-at-point))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * soma-untabify-and-delete-trailing-whitespace
;;
(defun soma-untabify-and-delete-trailing-whitespace ()
  (interactive)
  (save-excursion
    (progn
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace))))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * markdown-link-to-org-link
;;
(require 'rx)
(require 's)
(defun soma-markdown-link-to-org-link (beg end)
  "Convert a link from markdown syntax to org-mode syntax

BEG and END are the bounds of the currently active region."
  (interactive "r")
  (cl-assert (integerp beg))
  (cl-assert (integerp end))
  (cl-assert (>= beg 0))
  (cl-assert (<= end (point-max)))
  (if (region-active-p)
    (let* ((region (buffer-substring-no-properties beg end)))
      (if
          (and
           (string-match
            (rx
             "["
             (group
              (zero-or-more anything))
             "]"
             "("
             (group
              (zero-or-more anything))
             ")")
            region)
           (match-beginning 1)
           (match-beginning 2))
          (let* ((description-beginning (match-beginning 1))
                 (description-end (match-end 1))
                 (description (substring
                               region
                               description-beginning
                               description-end))
                 (url-beginning (match-beginning 2))
                 (url-end (match-end 2))
                 (url (substring
                       region
                       url-beginning
                       url-end))
                 (org-mode-link (format
                                 "[[%s][%s]]"
                                 url
                                 description)))
            (delete-region beg end)
            (insert org-mode-link))
        (message "Not a valid markdown link")))
    (message "No active region found!")))
;;
(defun soma-msg (msg)
  (message "%s" msg))
;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; * soma-utils-customize-set-variable
;;
;; This function acts just like customize-set-variable,
;; except that it will take the name of a variable to set,
;; rather than prompting the user for it.

(defun soma-utils-customize-set-variable (variable)
  "Set the default for VARIABLE to to a user-selected VALUE, and return VALUE.
VALUE is a Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive)
  (cl-flet ((soma-utils--custom-prompt-variable
              (var prompt-val &optional comment)
                "Prompt for a given variable's value and return
              both as a list.
              PROMPT-VAL is the prompt for the value.
              The %s escape in PROMPT-VAL is replaced with
              the name of the variable.

              If the variable has a `variable-interactive'
              property, that is used as if it were the arg to
              `interactive' (which see) to interactively read the
              value.

              If the variable has a `custom-type' property, it
              must be a widget and the `:prompt-value' property
              of that widget will be used for reading the value.
              If the variable also has a `custom-get' property,
              that is used for finding the current value of the
              variable, otherwise `symbol-value' is used.

              If optional COMMENT argument is non-nil, also
              prompt for a comment and return
              it as the third element in the list."
                (let* ((minibuffer-help-form '(describe-variable var))
                       (val
                        (let ((prop (get var 'variable-interactive))
		              (type (get var 'custom-type))
		              (prompt (format prompt-val var)))
	                  (unless (listp type)
	                    (setq type (list type)))
	                  (cond (prop
		                 ;; Use VAR's `variable-interactive' property
		                 ;; as an interactive spec for prompting.
		                 (call-interactively `(lambda (arg)
					                (interactive ,prop)
					                arg)))
		                (type
		                 (widget-prompt-value
                                  type
				  prompt
				  (if (boundp var)
                                      (funcall
                                       (or (get var 'custom-get) 'symbol-value)
                                       var))
				  (not (boundp var))))
		                (t
		                 (eval-minibuffer prompt))))))
                  (if comment
 	              (list var val
 	                    (read-string "Comment: " (get var 'variable-comment)))
                    (list var val)))))
    (let* ((var-value-comment (soma-utils--custom-prompt-variable
                               variable
                               "Set customized value for %s to: "
                               current-prefix-arg))
           (value (cl-second var-value-comment))
           (comment (cl-third var-value-comment)))
      (custom-load-symbol variable)
      (custom-push-theme 'theme-value variable 'user 'set (custom-quote value))
      (funcall (or (get variable 'custom-set) 'set-default) variable value)
      (put variable 'customized-value (list (custom-quote value)))
      (cond ((string= comment "")
             (put variable 'variable-comment nil)
             (put variable 'customized-variable-comment nil))
            (comment
             (put variable 'variable-comment comment)
             (put variable 'customized-variable-comment comment)))
      value)))
    ;;
;;------------------------------------------------------------------------

(defun soma-toggle-variable (variable)
  "Invert value of a given variable"
  (let ((set (or (get variable 'custom-set) 'set-default))
        (get (or (get variable 'custom-get) 'default-value)))
    (funcall set variable (not (funcall get variable)))))


;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;
;; See:  https://emacs.stackexchange.com/questions/46390/convert-durations-to-start-times
;;
(defun soma-duration-to-seconds (duration)
  "Return number of seconds of DURATION string."
  (cl-assert (stringp duration))
  (let ((seconds 0)
        (unit 1))
    (dolist (n (nreverse (mapcar #'string-to-number (split-string duration ":"))))
      (setq seconds (+ seconds (* n unit)))
      (setq unit (* unit 60)))
    seconds))

;(cl-assert (= (soma-duration-to-seconds "04") 4))
;(cl-assert (= (soma-duration-to-seconds "7:32") (+ (* 7 60) 32)))
;(cl-assert (= (soma-duration-to-seconds "1:09:27") 4167))

(defun soma-format-seconds (seconds)
  (cl-assert (integerp seconds))
  (let* ((h (floor seconds 3600))
         (seconds (- seconds (* 3600 h)))
         (m (floor seconds 60))
         (seconds (- seconds (* 60 m)))
         (s seconds))
    (if (zerop h)
        (format "%d:%02d" m s)
      (format "%d:%02d:%02d" h m s))))

;(cl-assert (string= (soma-format-seconds (soma-duration-to-seconds "0")) "0:00"))
;(cl-assert (string= (soma-format-seconds (soma-duration-to-seconds "00:09:26")) "9:26"))

(defun soma-durations-to-start-times (durations)
  (cl-assert (listp durations))
  (let ((start 0))
    (mapcar (lambda (duration)
              (cl-assert (integerp duration))
              (prog1 (soma-format-seconds start)
                (setq start (+ start (soma-duration-to-seconds duration)))))
            durations)))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--convert-tab-separated-table-to-org-format ()
  "Convert a TAB-separated table to org-mode table format"
  (interactive)
  (when (region-active-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (region-contents (buffer-substring-no-properties start end))
           (modified-text (with-temp-buffer
                            (insert region-contents)
                            (org-table-convert-region (point-min) (point-max))
                            (org-mode)
                            (goto-char (point-min))
                            (dotimes (_ (count-lines (point-min) (point-max)))
                              (org-table-hline-and-move))
                            (delete-region (line-beginning-position) (line-end-position))
                            (forward-line -1)
                            (let ((current-line-contents
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                              (goto-char (point-min))
                              (insert current-line-contents)
                              (insert "\n"))
                            (buffer-string))))
      (delete-region start end)
      (goto-char start)
      (insert modified-text))))

;;
;;------------------------------------------------------------------------

;;------------------------------------------------------------------------
;;

(defun soma--paste-from-x-without-backquotes-wrapping ()
  "Paste from X after removing the surrounding backquote fences.

LLMs like to wrap their commit messages in fences made of three
backquotes like ```.  This function will paste from X after removing
the backquote wrapping and anything outside of them.

For example, if the X selection contains:

```
foo
```

bar


will be pasted in as:

foo"
  (interactive)
  (save-excursion
    (let* ((modified-text
            (with-temp-buffer
              (soma-paste-from-clipboard)
              (save-excursion
                (save-restriction
                  (widen)

                  (let ((fence-regexp "^\\s-*```.*$")
                        (extraction-successful nil))

                    (condition-case nil
                        (progn
                          ;; 1. Search for the **first** opening fence.
                          (goto-char (point-min))
                          (unless (re-search-forward fence-regexp nil t)
                            (error "No opening fence found."))

                          ;; --- Opening Fence Found ---
                          (let ((open-fence-start (match-beginning 0))
                                (open-fence-end (match-end 0)))

                            ;; 2. Search for the **second (closing)** fence, starting immediately after the first one.
                            (goto-char open-fence-end)
                            (unless (re-search-forward fence-regexp nil t)
                              (error "Missing subsequent closing fence."))

                            ;; --- Closing Fence Found ---
                            (let ((close-fence-start (match-beginning 0)))

                              ;; 3. Perform Deletions:

                              ;; A) Delete the second fence and everything after it.
                              (delete-region close-fence-start (point-max))

                              ;; B) Delete the first fence.
                              (delete-region open-fence-start open-fence-end)

                              ;; C) Delete the text above the first fence.
                              (delete-region (point-min) open-fence-start)

                              ;; 4. Remove Trailing Newline (The Fix!)
                              (goto-char (point-max))
                              ;; Check if the character just before the end of the buffer is a newline.
                              (when (and (> (point-max) (point-min))
                                         (= (char-before (point-max)) ?\n))
                                (delete-char -1))

                              (setq extraction-successful t)
                              ))

                          (message "Successfully extracted content inside the first Markdown block."))

                      ;; Catch block handles the errors.
                      (error
                       (message "Extraction failed or cancelled (No fences found). Buffer unchanged.")
                       nil))

                    (unless extraction-successful
                      (message "No changes were made to the buffer."))
                    )))
              (buffer-string))))
      (evil-with-single-undo (insert modified-text)))))

;;
;;------------------------------------------------------------------------

(provide 'soma-utils)
