;;; tex-parens.el --- like lisp.el but for tex  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: tex, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides commands for working with lists, sexps and
;; defuns, much like those in lisp.el, but for tex buffers.  Here, the
;; role of parentheses is played by begin/end blocks, math delimiters
;; and mathematical parenthesis-like constructions (e.g.,
;; langle/rangle), together with their tex modifiers (e.g.,
;; left/right, bigl/bigr).  See README.org for more details.

;; There are many packages that aim to provide such features for
;; general modes, but I was unable to find one that provides the
;; desired functionality for the special case of tex buffers.

;; Sample configuration:
;;
;; (use-package tex-parens
;;   :ensure (:host github :repo "ultronozm/tex-parens.el"
;;                  :depth nil)
;;   :bind
;;   (:map LaTeX-mode-map
;;         ("C-M-f" . tp-forward-sexp)
;;         ("C-M-b" . tp-backward-sexp)
;;         ("C-M-n" . tp-forward-list)
;;         ("C-M-p" . tp-backward-list)
;;         ("C-M-u" . tp-backward-up-list)
;;         ("M-u" . tp-up-list)
;;         ("C-M-g" . tp-down-list)
;;         ("M-_" . tp-delete-pair)
;;         ("C-M-SPC" . tp-mark-sexp)
;;         ("C-M-k" . tp-kill-sexp)
;;         ("C-M-t" . transpose-sexps)
;;         ("C-M-<backspace>" . tp-backward-kill-sexp)
;;         ("M-+" . tp-raise-sexp))
;;   :hook
;;   (LaTeX-mode . tp-setup))

;;; Code:

(require 'cl-lib)

(defgroup tex-parens nil
  "Like lisp.el but for tex."
  :group 'tex
  :prefix "tp-")

(defun tp--beginning-of-defun ()
  "Move to the beginning of the current defun.
Here `defun' means top-level environment."
  (interactive)
  (re-search-backward "^\\\\begin{[^}]+}" nil t))

(defun tp--end-of-defun ()
  "Move to the end of the current defun.
Here `defun' means top-level environment."
  (interactive)
  (re-search-forward "^\\\\end{[^}]+}.*\n" nil t))

(defcustom tp-left-right-delimiter-pairs
  '(("(" . ")")
    ("[" . "]")
    ("\\{" . "\\}")
    ("\\langle" . "\\rangle")
    ("\\lvert" . "\\rvert")
    ("\\lVert" . "\\rVert")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil"))
  "Left/right pairs of delimiters."
  :type '(repeat (cons string string)))

(defcustom tp-solo-delimiters
  '("|" "\\|" "\\vert" "\\Vert")
  "Delimiters that do come in pairs."
  :type '(repeat string))

(defcustom tp-left-right-modifier-pairs
  '(("\\left" . "\\right")
    ("\\bigl" . "\\bigr")
    ("\\Bigl" . "\\Bigr")
    ("\\biggl" . "\\biggr")
    ("\\Biggl" . "\\Biggr"))
  "Left/right pairs of delimiter modifiers."
  :type '(repeat (cons string string)))

(defcustom tp-solo-modifiers
  '("\\big" "\\Big" "\\bigg" "\\Bigg")
  "Delimiter modifiers that do not come in pairs."
  :type '(repeat string))

(defcustom tp-other-parens
  '(("``" . "''")
    ("{" . "}")
    ("\\(" . "\\)")
    ("\\[" . "\\]")
    ("\\left." . "\\right."))
  "Left/right delimiters not to be combined with modifiers."
  :type '(repeat (cons string string)))

(defcustom tp-max-delim-length 25
  "Maximum length of a delimiter.
This is comfortably larger than `\biggl\langle' and
`\begin{proposition}', for example."
  :type 'integer)

(defun tp--reduce-append (func list1 list2)
  "List consisting of FUNC applied to pairs from LIST1 and LIST2."
  (cl-reduce #'append
             (mapcar (lambda (item1)
                       (mapcar (lambda (item2)
                                 (funcall func item1 item2))
                               list2))
                     list1)))

(defun tp--generate-pairs ()
  "Generate list of left/right pairs of delimiters.
With the exception of the math delimiters `$' and `$$', we only
form delimiters which are visibly `left'/`opening' or
`right'/`closing'."
  (let ((unambiguous-parens tp-left-right-delimiter-pairs)
        (ambiguous-parens tp-solo-delimiters)
        (unambiguous-modifiers tp-left-right-modifier-pairs)
        (ambiguous-modifiers tp-solo-modifiers))
    (append
     tp-other-parens
     '(("$" . "$")
       ("$$" . "$$"))
     unambiguous-parens
     (tp--reduce-append
      (lambda (up um)
        (cons (concat (car um) (car up))
              (concat (cdr um) (cdr up))))
      unambiguous-parens unambiguous-modifiers)
     (tp--reduce-append
      (lambda (up m)
        (cons (concat m (car up))
              (concat m (cdr up))))
      unambiguous-parens ambiguous-modifiers)
     (tp--reduce-append
      (lambda (p um)
        (cons (concat (car um) p)
              (concat (cdr um) p)))
      ambiguous-parens unambiguous-modifiers))))

(defvar tp--pairs nil)
(defvar tp--pairs-swap nil)
(defvar tp--delims nil)
(defvar tp--regexp nil)
(defvar tp--regexp-open nil)
(defvar tp--regexp-close nil)
(defvar tp--regexp-reverse nil)

(defvar preview-auto-reveal)

(defun tp-setup ()
  "Set up tex-parens.  Intended as a hook for `LaTeX-mode'."
  (setq
   preview-auto-reveal
   '(eval (preview-arrived-via (key-binding [left])
                               (key-binding [right])
                               #'backward-char #'forward-char
                               #'tp-down-list
                               #'tp-backward-down-list)))
  (when (boundp 'TeX-fold-auto-reveal)
    (setq TeX-fold-auto-reveal
          '(eval (preview-arrived-via (key-binding [left])
                                      (key-binding [right])
                                      #'backward-char #'forward-char
                                      #'mouse-set-point
                                      #'tp-down-list
                                      #'tp-backward-down-list
                                      #'tp-forward-list
                                      #'tp-backward-list
                                      #'tp-up-list
                                      #'tp-backward-up-list
                                      #'tp-forward-sexp
                                      #'tp-backward-sexp))))
  (setq-local beginning-of-defun-function #'tp--beginning-of-defun)
  (setq-local transpose-sexps-default-function #'tp-transpose-sexps-default-function)
  (setq end-of-defun-function #'tp--end-of-defun)
  (setq tp--pairs (tp--generate-pairs))
  (setq tp--pairs-swap
        (mapcar (lambda (x) (cons (cdr x) (car x))) tp--pairs))
  (setq tp--delims (append (mapcar #'car tp--pairs)
                           (mapcar #'cdr tp--pairs)))
  (setq tp--regexp
        (concat (regexp-opt tp--delims)
                "\\|\\\\begin{[^}]+}"
                "\\|\\\\end{[^}]+}"
                "\\|\\\\[a-zA-Z]+\\[[^]]+\\]{"
                "\\|\\\\[a-zA-Z]+{"
                ))
  (setq tp--regexp-open
        (concat (regexp-opt (mapcar #'car tp--pairs))
                "\\|\\\\begin{[^}]+}"
                "\\|\\\\[a-zA-Z]+\\[[^]]+\\]{"
                "\\|\\\\[a-zA-Z]+{"))
  (setq tp--regexp-close
        (concat (regexp-opt (mapcar #'cdr tp--pairs))
                "\\|\\\\end{[^}]+}"))
  (setq tp--regexp-reverse
        (concat "}[^{]+{nigeb\\\\\\|"
                "}[^{]+{dne\\\\\\|"
                "{[a-zA-Z]+\\\\\\|"
                "{\\][^]]+\\[[a-zA-Z]+\\\\\\|"
                (regexp-opt (mapcar #'reverse tp--delims))))

  ;; Don't know why, but Emacs freezes with the following line
  ;; uncommented.  For this reason, we have to go through the
  ;; awkwardness of duplicating several functions near the bottom of
  ;; this file.

  ;; (setq-local forward-sexp-function #'tp-forward-sexp)
  )

(defcustom tp-search-limit 10000
  "How far to search for a delimiter, in either direction.
This should exceed the length, in characters, of the longest
theorem-like environments to which you care about applying the
list and sexp-based navigation commands.  Longer environments
typically occur at the top level and are best navigated using the
defun-based commands."
  :type 'integer
  :group 'tex-parens)

(defun tp--math-face ()
  "Returns the number of math face modifiers at point.
0 means no math face."
  (let ((face (plist-get (text-properties-at (point))
                         'face)))
    (cond
     ((memq face '(tex-math font-latex-math-face))
      1)
     ((listp face)
      (let ((total 0))
        (dolist (f face)
          (when (memq f '(tex-math font-latex-math-face))
            (setq total (1+ total))))
        total))
     (t 0))))

(defcustom tp-ignore-comments t
  "Whether to ignore comments when searching for delimiters."
  :type 'boolean
  :group 'tex-parens)

(defun tp--ignore (m-str m-begin m-end)
  "Check if M-STR should be ignored.
M-STR is the string matched by the search, while M-BEGIN and
M-END delimit the match.  If `tp-ignore-comments' is non-nil,
then ignore comments; these are detected via
`font-lock-comment-face'.  If M-STR is a double prime in math
mode, then ignore it.  If M-STR is a dollar delimiter that does
not demarcate math mode, then ignore it."
  (or
   (and
    tp-ignore-comments
    (let ((face (plist-get (text-properties-at (point))
                           'face)))
      (eq face 'font-lock-comment-face)))
   (and
    ;; ignore double prime in math-mode
    (equal m-str "''")
    (> (tp--math-face) 0))
   (and
    ;; ignore dollar delimiters that do not demarcate math mode
    (member m-str '("$" "$$"))
    (equal
     (save-excursion
       (goto-char (1- m-begin))
       (tp--math-face))
     (save-excursion
       (goto-char m-end)
       (tp--math-face))))))

(defun tp--search-forward (regexp bound)
  "Search forward for REGEXP up to BOUND."
  (let (success done)
    (while (not done)
      (if (re-search-forward regexp bound t)
          (when (not (tp--ignore (match-string 0)
                                 (match-beginning 0)
                                 (match-end 0)))
            (setq done t
                  success t))
        (setq done t)))
    (when success
      (match-string 0))))

(defun tp--search-backward (regexp regexp-reverse bound)
  "Search backward, greedily, for REGEXP up to BOUND.
Assumes that REGEXP-REVERSE is the reverse of REGEXP."
  (let (done success match)
    (while (not done)
      (if (re-search-backward regexp bound t)
          (progn
            ;; find the longest match by applying the backward regexp
            ;; to the reversed text
            (goto-char (match-end 0))
            (let ((text (buffer-substring-no-properties
                         (max (point-min)
                              (- (point) tp-max-delim-length))
                         (point))))
              (with-temp-buffer
                (insert (reverse text))
                (goto-char (point-min))
                (re-search-forward regexp-reverse nil t)
                (setq match (reverse (match-string 0))))
              (backward-char (length match))
              (when (not (tp--ignore match
                                     (point)
                                     (+ (point) (length match))))
                (setq done t)
                (setq success t))))
        ;; didn't find anything, so we failed
        (setq done t)))
    (when success
      match)))

(defun tp--forward-bound ()
  "Return the default bound for forward search."
  (save-excursion
    (min
     (point-max)
     (+ (point) tp-search-limit))))

(defun tp--backward-bound ()
  "Return the default bound for backward search."
  (save-excursion
    (max
     (point-min)
     (- (point) tp-search-limit))))

(defun tp--forward-delim (&optional bound)
  "Search for the next delimiter up to BOUND.
Return the delimiter found, or nil if none is found."
  (unless bound (setq bound (tp--forward-bound)))
  (tp--search-forward tp--regexp bound))

(defun tp--backward-delim (&optional bound)
  "Search for the previous delimiter up to BOUND.
Return the delimiter found, or nil if none is found."
  (unless bound (setq bound (tp--backward-bound)))
  (tp--search-backward tp--regexp tp--regexp-reverse bound))

(defun tp--close-of-open (delim)
  "Check if DELIM is opening, return the corresponding closing.
If DELIM is an opening delimiter, return the corresponding closing
delimiter.  Otherwise, return nil."
  (or
   (cdr (assoc delim tp--pairs))
   (and (stringp delim)
        (or
         (and (string-match "\\\\begin{\\([^}]+\\)}" delim)
              (let ((type (match-string 1 delim)))
                (format "\\end{%s}" type)))
         (unless (string-match "\\\\end{\\([^}]+\\)}" delim)
           (and (string-match "\\\\[a-zA-Z]+\\[[^]]+\\]{\\|\\\\[a-zA-Z]+{" delim)
                "}"))))))

(defun tp--open-of-close (delim)
  "Check if DELIM is closing, return the corresponding opening.
If DELIM is a closing delimiter, return the corresponding opening
delimiter.  Otherwise, return nil."
  (or
   (cdr (assoc delim tp--pairs-swap))
   (and (stringp delim)
        (string-match "\\\\end{\\([^}]+\\)}" delim)
        (let ((type (match-string 1 delim)))
          (format "\\begin{%s}" type)))))

(defvar tp--debug nil)

(defun tp--forward-search-found-open (delim)
  "Check if DELIM is an opening delimiter found by forward search."
  (cond
   ((member delim '("$" "$$"))
    (>
     (tp--math-face)
     (save-excursion
       (backward-char (1+ (length delim)))
       (tp--math-face))
     ))
   (t
    (tp--close-of-open delim))))

(defun tp--backward-search-found-close (delim)
  "Check if DELIM is a closing delimiter found by backward search."
  (cond
   ((member delim '("$" "$$"))
    (>
     (save-excursion
       (backward-char 1)
       (tp--math-face))
     (save-excursion
       (forward-char (length delim))
       (tp--math-face))))
   (t
    (tp--open-of-close delim))))

(defun tp--check-match (delim other stack-top)
  "Internal function used for debugging.
Check that OTHER is non-nil.  This should always be the case.
Then, if debugging is enabled, check whether STACK-TOP and DELIM
coincide.  Sometimes this is intentional (e.g., when `\right.'
terminates `\left{'), so we do not treat it as an error."
  (cl-assert other)
  (when tp--debug
    (unless (equal other stack-top)
      (message "Mismatched delimiters: %s %s" stack-top delim))))

(defun tp-forward-list (&optional arg)
  "Move forward across one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward across N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tp--forward-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tp--backward-list-1)
    (setq arg (1+ arg))))

(defun tp-backward-list (&optional arg)
  "Move backward across one balanced group.
With ARG, do it that many times.  Negative
arg -N means move forward across N balanced groups."
  (interactive "^p")
  (tp-forward-list (- (or arg 1))))

(defun tp--forward-list-1 (&optional bound)
  "Move forward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tp--forward-bound)))
  (let ((start (point))
        (delim (tp--forward-delim bound))
        (stack ())
        success)
    (while delim
      (if (tp--forward-search-found-open delim)
          (push delim stack)
        (if stack
            (progn
              (tp--check-match delim (tp--open-of-close delim) (car stack))
              (pop stack)
              (unless stack
                (setq success t)))
          (backward-char (length delim))
          (setq success t)))
      (setq delim (and (not success) (tp--forward-delim bound))))
    (unless success
      (goto-char start)
      (when tp--debug
        (message "Unmatched delimiters: %s" (car stack))))))

(defun tp--backward-list-1 (&optional bound)
  "Move backward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tp--backward-bound)))
  (let ((start (point))
        (delim (tp--backward-delim bound))
        (stack ())
        success)
    (while delim
      (if (tp--backward-search-found-close delim)
          (push delim stack)
        (if stack
            (progn
              (tp--check-match delim (tp--close-of-open delim) (car stack))
              (pop stack)
              (unless stack
                (setq success t)))
          (forward-char (length delim))
          (setq success t)))
      (setq delim (and (not success) (tp--backward-delim bound))))
    (unless success
      (goto-char start)
      (when tp--debug
        (message "Unmatched delimiters: %s" (car stack))))))

(defun tp-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
If `forward-sexp' does not take us past the starting point of the
next delimiter, then do that.  Otherwise, do
`tp-forward-list'.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tp--forward-sexp-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tp--backward-sexp-1)
    (setq arg (1+ arg))))

(defun tp-backward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
If `forward-sexp' does not take us past the starting point of the
next delimiter, then do that.  Otherwise, do
`tp-forward-list'.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions."
  (interactive "^p")
  (tp-forward-sexp (- (or arg 1))))

(defun tp--forward-sexp-1 ()
  "Move forward across one balanced expression (sexp).
Helper function for `tp-forward-sexp'."
  (let ((delim-beg (save-excursion
                     (tp--forward-delim)
                     (match-beginning 0)))
        (vanilla
         (condition-case _
             (save-excursion
               (goto-char (or (scan-sexps (point) 1) (buffer-end 1)))
               (point))
           (scan-error nil))))
    (if (or (not vanilla)
            (and delim-beg
                 (> vanilla delim-beg)))
        (tp--forward-list-1)
      (goto-char vanilla))))

(defun tp--backward-sexp-1 ()
  "Move backward across one balanced expression (sexp).
If `backward-sexp' does not take us beyond the ending point of
the previous delimiter, then do that.  Otherwise, do
`tp-backward-list'."
  (interactive)
  (let ((delim-end (save-excursion
                     (when-let ((delim (tp--backward-delim)))
                       (forward-char (length delim))
                       (point))))
        (vanilla
         (condition-case _
             (save-excursion
               (goto-char (or (scan-sexps (point) -1) (buffer-end -1)))
               (backward-prefix-chars)
               (point))
           (scan-error nil))))
    (if (or (not vanilla)
            (and delim-end
                 (< vanilla delim-end)))
        (tp--backward-list-1)
      (goto-char vanilla))))

(defun tp-up-list (&optional arg)
  "Move forward out of one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward out of N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tp--up-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tp--backward-up-list-1)
    (setq arg (1+ arg))))

(defun tp-backward-up-list (&optional arg)
  "Move backward out of one balanced group.
With ARG, do it that many times.  Negative arg -N means move
forward out of N balanced groups."
  (interactive "^p")
  (tp-up-list (- (or arg 1))))

(defun tp--up-list-1 (&optional bound)
  "Move forward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tp--forward-bound)))
  (let ((start (point))
        success
        (delim (tp--forward-delim bound))
        (stack ()))
    (while delim
      (if (tp--forward-search-found-open delim)
          (push delim stack)
        (if stack
            (progn
              (tp--check-match delim (tp--open-of-close delim) (car stack))
              (pop stack))
          (setq success t)))
      (setq delim (and (not success) (tp--forward-delim bound))))
    (unless success
      (goto-char start))))

(defun tp--backward-up-list-1 (&optional bound)
  "Move backward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tp--backward-bound)))
  (let ((start (point))
        success
        (delim (tp--backward-delim bound))
        (stack ()))
    (while delim
      (if (tp--backward-search-found-close delim)
          (push delim stack)
        (if stack
            (progn
              (tp--check-match delim (tp--close-of-open delim) (car stack))
              (pop stack))
          (setq success t)))
      (setq delim (and (not success) (tp--backward-delim bound))))
    (unless success
      (goto-char start))))

(defun tp-down-list (&optional arg)
  "Move forward into one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward into N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tp--down-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tp--backward-down-list-1)
    (setq arg (1+ arg))))

(defun tp-backward-down-list (&optional arg)
  "Move backward into one balanced group.
With ARG, do it that many times.  Negative arg -N means move
forward into N balanced groups."
  (interactive "^p")
  (tp-down-list (- (or arg 1))))

(defun tp--down-list-1 (&optional bound)
  "Move forward into one balanced group.
Search up to BOUND.  Return t if successful, nil otherwise."
  (interactive)
  (unless bound (setq bound (tp--forward-bound)))
  (let ((start (point))
        (delim (tp--forward-delim bound))
        success)
    (when (and delim
               (tp--forward-search-found-open delim))
      (setq success t))
    (unless success
      (goto-char start))
    (when (fboundp 'preview-move-point)
      (preview-move-point))
    success))

(defun tp--backward-down-list-1 (&optional bound)
  "Move backward into one balanced group.
Search up to BOUND.  Return t if successful, nil otherwise."
  (interactive)
  (unless bound (setq bound (tp--backward-bound)))
  (let ((start (point))
        (delim (tp--backward-delim bound))
        success)
    (when (and delim
               (tp--backward-search-found-close delim))
      (setq success t))
    (unless success
      (goto-char start))
    (when (fboundp 'preview-move-point)
      (preview-move-point))
    success))

(defun tp-delete-pair ()
  "Delete a balanced pair of delimiters that follow point.
Push a mark at the end of the contents of the pair."
  (interactive)
  (when (tp--down-list-1)
    (save-excursion
      (tp-up-list)
      (let ((q (point)))
        (tp--backward-delim)
        (delete-region (point) q)
        (push-mark)))
    (let ((q (point)))
      (tp--backward-delim)
      (delete-region (point) q))))

;;; AWKWARDNESS BEGINS HERE

;; it shouldn't be necessary to define any of the following (via
;; copy/paste from lisp.el and simple.el) -- it should suffice to set
;; forward-sexp-function to tp-forward-sexp -- but for some
;; reason, Emacs freezes when I do so.  I haven't been able to debug
;; why.  alas.

(defun tp-mark-sexp (&optional arg allow-extend)
  "Set mark ARG sexps from point or move mark one sexp.
When called from Lisp with ALLOW-EXTEND omitted or nil, mark is
set ARG sexps from point.
With ARG and ALLOW-EXTEND both non-nil (interactively, with prefix
argument), the place to which mark goes is the same place \\[forward-sexp]
would move to with the same argument; if the mark is active, it moves
ARG sexps from its current position, otherwise it is set ARG sexps
from point.
When invoked interactively without a prefix argument and no active
region, mark moves one sexp forward.
When invoked interactively without a prefix argument, and region
is active, mark moves one sexp away of point (i.e., forward
if mark is at or after point, back if mark is before point), thus
extending the region by one sexp.  Since the direction of region
extension depends on the relative position of mark and point, you
can change the direction by \\[exchange-point-and-mark].
This command assumes point is not in a string or comment."
  (interactive "P\np")
  (cond ((and allow-extend
	             (or (and (eq last-command this-command) (mark t))
		                (and transient-mark-mode mark-active)))
	        (setq arg (if arg (prefix-numeric-value arg)
		                   (if (< (mark) (point)) -1 1)))
	        (set-mark
	         (save-excursion
	           (goto-char (mark))
            (condition-case error
	               (tp-forward-sexp arg)
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No more sexp to select"
                             (cadr error)))))
	           (point))))
	       (t
	        (push-mark
	         (save-excursion
            (condition-case error
	               (tp-forward-sexp (prefix-numeric-value arg))
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No sexp to select"
                             (cadr error)))))
	           (point))
	         nil t))))

(defun tp-kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) following point.
With ARG, kill that many sexps after point.
Negative arg -N means kill N sexps before point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (if interactive
      (condition-case _
          (tp-kill-sexp arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next sexp"
                                  "No previous sexp"))))
    (let ((opoint (point)))
      (tp-forward-sexp (or arg 1))
      (kill-region opoint (point)))))

(defun tp-backward-kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) preceding point.
With ARG, kill that many sexps before point.
Negative arg -N means kill N sexps after point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (tp-kill-sexp (- (or arg 1)) interactive))

(defun tp-transpose-sexps-default-function (arg)
  "Default method to locate a pair of points for `transpose-sexps'.
ARG is as in the docstring for `transpose-sexps'."
  ;; Here we should try to simulate the behavior of
  ;; (cons (progn (forward-sexp x) (point))
  ;;       (progn (forward-sexp (- x)) (point)))
  ;; Except that we don't want to rely on the second forward-sexp
  ;; putting us back to where we want to be, since forward-sexp-function
  ;; might do funny things like infix-precedence.
  (if (if (> arg 0)
	         (looking-at "\\sw\\|\\s_")
	       (and (not (bobp))
	            (save-excursion
               (forward-char -1)
               (looking-at "\\sw\\|\\s_"))))
      ;; Jumping over a symbol.  We might be inside it, mind you.
      (progn (funcall (if (> arg 0)
			                       #'skip-syntax-backward #'skip-syntax-forward)
		                    "w_")
	            (cons (save-excursion (tp-forward-sexp arg) (point)) (point)))
    ;; Otherwise, we're between sexps.  Take a step back before jumping
    ;; to make sure we'll obey the same precedence no matter which
    ;; direction we're going.
    (funcall (if (> arg 0) #'skip-syntax-backward #'skip-syntax-forward)
             " .")
    (cons (save-excursion (tp-forward-sexp arg) (point))
	         (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			                         (not (zerop (funcall (if (> arg 0)
						                                               #'skip-syntax-forward
						                                             #'skip-syntax-backward)
						                                           ".")))))
		               (point)))))

(defun tp-raise-sexp (&optional n)
  "Raise N sexps one level higher up the tree.

This function removes the sexp enclosing the form which follows
point, and then re-inserts N sexps that originally followed point,
thus raising those N sexps one level up.

Interactively, N is the numeric prefix argument, and defaults to 1.

For instance, if you have:

  (let ((foo 2))
    (progn
      (setq foo 3)
      (zot)
      (+ foo 2)))

and point is before (zot), \\[raise-sexp] will give you

  (let ((foo 2))
    (zot))"
  (interactive "p")
  (let ((s (if (and transient-mark-mode mark-active)
               (buffer-substring (region-beginning) (region-end))
             (buffer-substring
              (point)
              (save-excursion (tp-forward-sexp n) (point))))))
    (tp-backward-up-list)
    (delete-region (point) (save-excursion (tp-forward-sexp 1) (point)))
    (save-excursion (insert s))))

;;; BURP

(defun tp--slurp-left ()
  "Slurp the next sexp into the current one, to the left."
  (when-let ((pos (point))
             (match (when (looking-at tp--regexp) (match-string 0))))
    (delete-region (point) (+ (point) (length match)))
    (condition-case nil
        (progn
          (tp-backward-sexp)
          (insert match)
          (backward-char (length match)))
      (error
       (goto-char pos)
       (insert match)
       (backward-char (length match))))))

(defun tp--barf-left ()
  "Barf the next sexp out of the current one, to the right."
  (when-let* ((pos (point))
              (bound (max (point-min)
                          (- (point) tp-max-delim-length)))
              (text (buffer-substring bound pos))
              (reversed-text (reverse text))
              (reverse-match
               (with-temp-buffer
                 (insert reversed-text)
                 (goto-char (point-min))
                 (when (looking-at tp--regexp-reverse)
                   (match-string 0))))
              (match (reverse reverse-match)))
    (backward-char (length match))
    (progn
      (tp-backward-sexp)
      (let ((q
             (save-excursion
               (tp-forward-sexp)
               (point))))
        (tp-backward-sexp)
        (when (not (equal q
                          (save-excursion
                            (tp-forward-sexp)
                            (point))))
          (tp-forward-sexp))))
    (insert match)
    (save-excursion
      (goto-char pos)
      (delete-char (length match)))))

(defun tp-burp-left ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((and
     (not (looking-back tp--regexp-open (max (point-min) (- (point) tp-max-delim-length))))
     (looking-back tp--regexp-close (max (point-min) (- (point) tp-max-delim-length))))
    (tp--barf-left))
   ((and
     (not (looking-at tp--regexp-close))
     (looking-at tp--regexp-open))
    (tp--slurp-left))
   (t
    (call-interactively #'self-insert-command))))

(defun tp--barf-right ()
  "Barf the next sexp out of the current one, to the right."
  (let ((pos (point))
        (match (when (looking-at tp--regexp) (match-string 0))))
    (forward-char (length match))
    (progn
      (tp-forward-sexp)
      (let ((q
             (save-excursion
               (tp-backward-sexp)
               (point))))
        (tp-forward-sexp)
        (when (not (equal q
                          (save-excursion
                            (tp-backward-sexp)
                            (point))))
          (tp-backward-sexp))))
    (insert match)
    (save-excursion
      (goto-char pos)
      (delete-char (length match)))
    (backward-char (length match))))

(defun tp--slurp-right ()
  "Slurp the next sexp into the current one, to the right."
  (when-let* ((pos (point))
              (bound (max (point-min)
                          (- (point) tp-max-delim-length)))
              (text (buffer-substring bound pos))
              (reversed-text (reverse text))
              (reverse-match
               (with-temp-buffer
                 (insert reversed-text)
                 (goto-char (point-min))
                 (when (looking-at tp--regexp-reverse)
                   (match-string 0))))
              (match (reverse reverse-match)))
    (condition-case nil
        (progn
          (tp-forward-sexp)
          (insert match)
          (save-excursion
            (goto-char pos)
            (backward-char (length match))
            (delete-char (length match))))
      (error nil))))

(defun tp-burp-right ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((and
     (not (looking-back tp--regexp-open (max (point-min) (- (point) tp-max-delim-length))))
     (looking-back tp--regexp-close (max (point-min) (- (point) tp-max-delim-length))))
    (tp--slurp-right))
   ((and
     (not (looking-at tp--regexp-close))
     (looking-at tp--regexp-open))
    (tp--barf-right))
   (t
    (call-interactively #'self-insert-command))))

(provide 'tex-parens)
;;; tex-parens.el ends here
