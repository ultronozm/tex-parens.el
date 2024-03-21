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

;; This package provides sexp and list-based navigation, much like
;; those in lisp.el, but for tex modes, tailored to my use cases
;; (cf. https://github.com/Fuco1/smartparens/issues/1193).

;; Sample configuration:
;;
;; (use-package tex-parens
;;   :ensure (:host github :repo "ultronozm/tex-parens.el"
;;                  :depth nil)
;;   :bind
;;   (:map LaTeX-mode-map
;;         ("C-M-f" . tex-parens-forward-sexp)
;;         ("C-M-b" . tex-parens-backward-sexp)
;;         ("C-M-n" . tex-parens-forward-list)
;;         ("C-M-p" . tex-parens-backward-list)
;;         ("C-M-u" . tex-parens-backward-up-list)
;;         ("M-u" . tex-parens-up-list)
;;         ("C-M-g" . tex-parens-down-list)
;;         ("M-_" . tex-parens-delete-pair)
;;         ("C-M-SPC" . tex-parens-mark-sexp)
;;         ("C-M-k" . tex-parens-kill-sexp)
;;         ("C-M-t" . transpose-sexps)
;;         ("C-M-<backspace>" . tex-parens-backward-kill-sexp)
;;         ("M-+" . tex-parens-raise-sexp))
;;   :hook
;;   (LaTeX-mode . tex-parens-setup))

;;; Code:

(require 'cl-lib)

(defun tex-parens--beginning-of-defun ()
  "Move to the beginning of the current defun.
Here `defun' means top-level environment."
  (interactive)
  (re-search-backward "^\\\\begin{[^}]+}" nil t))

(defun tex-parens--end-of-defun ()
  "Move to the end of the current defun.
Here `defun' means top-level environment."
  (interactive)
  (re-search-forward "^\\\\end{[^}]+}\n" nil t))

(defun tex-parens--generate-pairs ()
  "Generate list of left/right pairs of delimiters."
  (let ((unambiguous-parens
         '(("(" . ")")
           ("[" . "]")
           ("\\{" . "\\}")
           ("\\langle" . "\\rangle")
           ("\\lvert" . "\\rvert")
           ("\\lVert" . "\\rVert")
           ("\\lfloor" . "\\rfloor")
           ("\\lceil" . "\\rceil")))
        (ambiguous-parens
         '("|" "\\|" "\\vert" "\\Vert"))
        (unambiguous-modifiers
         '(("\\left" . "\\right")
           ("\\bigl" . "\\bigr")
           ("\\Bigl" . "\\Bigr")
           ("\\biggl" . "\\biggr")
           ("\\Biggl" . "\\Biggr")))
        (ambiguous-modifiers
         '("\\big" "\\Big" "\\bigg" "\\Bigg"))
        (other-parens
         '(("``" . "''")
           ("$" . "$")
           ("{" . "}")
           ("$$" . "$$")
           ("\\(" . "\\)")
           ("\\[" . "\\]")
           ("\\left." . "\\right."))))
    (append
     other-parens unambiguous-parens
     (cl-reduce #'append
                (mapcar
                 (lambda (unam-par)
                   (mapcar
                    (lambda (unam-mod)
                      (cons (concat (car unam-mod) (car unam-par))
                            (concat (cdr unam-mod) (cdr unam-par))))
                    unambiguous-modifiers))
                 unambiguous-parens))
     (cl-reduce #'append
                (mapcar
                 (lambda (unam-par)
                   (mapcar
                    (lambda (am-mod)
                      (cons (concat am-mod (car unam-par))
                            (concat am-mod (cdr unam-par))))
                    ambiguous-modifiers))
                 unambiguous-parens))
     (cl-reduce #'append
                (mapcar
                 (lambda (am-par)
                   (mapcar
                    (lambda (unam-mod)
                      (cons (concat (car unam-mod) am-par)
                            (concat (cdr unam-mod) am-par)))
                    unambiguous-modifiers))
                 ambiguous-parens)))))

(defvar tex-parens--pairs nil)
(defvar tex-parens--pairs-swap nil)
(defvar tex-parens--delims nil)
(defvar tex-parens--regexp nil)
(defvar tex-parens--regexp-reverse nil)

(defvar preview-auto-reveal)

(defun tex-parens-setup ()
  "Set up tex-parens.  Intended as a hook for `LaTeX-mode'."
  (setq
   preview-auto-reveal
   '(eval (preview-arrived-via (key-binding [left])
                               (key-binding [right])
                               #'backward-char #'forward-char #'tex-parens-down-list)))
  (setq-local beginning-of-defun-function #'tex-parens--beginning-of-defun)
  (setq-local transpose-sexps-default-function #'tex-parens-transpose-sexps-default-function)
  (setq end-of-defun-function #'tex-parens--end-of-defun)
  (setq tex-parens--pairs (tex-parens--generate-pairs))
  (setq tex-parens--pairs-swap
        (mapcar (lambda (x) (cons (cdr x) (car x))) tex-parens--pairs))
  (setq tex-parens--delims (append (mapcar #'car tex-parens--pairs)
                                   (mapcar #'cdr tex-parens--pairs)))
  (setq tex-parens--regexp
        (concat (regexp-opt tex-parens--delims)
                "\\|\\\\begin{[^}]+}\\|\\\\end{[^}]+}"))
  (setq tex-parens--regexp-reverse
        (concat "}[^{]+{nigeb\\\\\\|}[^{]+{dne\\\\\\|"
                (regexp-opt (mapcar #'reverse tex-parens--delims))))

  ;; Don't know why, but Emacs freezes with the following line
  ;; uncommented.  For this reason, we have to go through the
  ;; awkwardness of duplicating several functions near the bottom of
  ;; this file.

  ;; (setq-local forward-sexp-function #'tex-parens-forward-sexp)
  )

(defcustom tex-parens-search-limit 10000
  "How far to search for a delimiter, in either direction.
This should exceed the length, in characters, of the longest
theorem-like environments that you encounter in practice."
  :type 'integer
  :group 'tex-parens)

(defun tex-parens--forward-bound ()
  "Return the bound for forward search."
  (save-excursion
    (min
     (point-max)
     (+ (point) tex-parens-search-limit))))

(defun tex-parens--backward-bound ()
  "Return the bound for backward search."
  (save-excursion
    (max
     (point-min)
     (- (point) tex-parens-search-limit))))

(defun tex-parens--forward-delim (&optional bound)
  "Search for the next delimiter up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (when (re-search-forward tex-parens--regexp bound t)
    (match-string 0)))

(defun tex-parens--backward-delim (&optional bound)
  "Search for the previous delimiter up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let* ((text (buffer-substring-no-properties bound (point)))
         match result)
    (with-temp-buffer
      (insert (reverse text))
      (goto-char (point-min))
      (setq result (re-search-forward tex-parens--regexp-reverse nil t))
      (when result (setq match (match-string 0))))
    (when result
      (backward-char (1- result))
      (reverse match))))

(defun tex-parens--close-of-open (delim)
  "Check if DELIM is opening, return the corresponding closing.
If DELIM is an opening delimiter, return the corresponding closing
delimiter.  Otherwise, return nil."
  (or
   (cdr (assoc delim tex-parens--pairs))
   (and (stringp delim)
        (string-match "\\\\begin{\\([^}]+\\)}" delim)
        (let ((type (match-string 1 delim)))
          (format "\\end{%s}" type)))))

(defun tex-parens--open-of-close (delim)
  "Check if DELIM is closing, return the corresponding opening.
If DELIM is a closing delimiter, return the corresponding opening
delimiter.  Otherwise, return nil."
  (or
   (cdr (assoc delim tex-parens--pairs-swap))
   (and (stringp delim)
        (string-match "\\\\end{\\([^}]+\\)}" delim)
        (let ((type (match-string 1 delim)))
          (format "\\begin{%s}" type)))))

(defun tex-parens--math-face-p ()
  "Check if point is in a math face."
  (let ((face (plist-get (text-properties-at (point))
                         'face)))
    (or (eq face 'font-latex-math-face)
        (and (listp face)
             (memq 'font-latex-math-face face)))))

(defvar tex-parens--debug nil)

(defun tex-parens-forward-list (&optional bound)
  "Move forward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        (delim (tex-parens--forward-delim bound))
        (stack ()))
    (while delim
      (if (or (and (member delim '("$" "$$"))
                   (tex-parens--math-face-p))
              (and (not (member delim '("$" "$$")))
                   (tex-parens--close-of-open delim)))
          (push delim stack)
        (let ((other (tex-parens--open-of-close delim)))
          (cl-assert other)
          (if stack
              (progn
                (when tex-parens--debug
                  (unless (equal other (car stack))
                    (message "Mismatched delimiters: %s %s" (car stack) delim)))
                (pop stack))
            (backward-char (length delim)))))
      (setq delim (and stack (tex-parens--forward-delim bound))))
    (when stack
      (goto-char start)
      (when tex-parens--debug
        (message "Unmatched delimiters: %s" (car stack))))))

(defun tex-parens-backward-list (&optional bound)
  "Move backward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let ((start (point))
        (delim (tex-parens--backward-delim bound))
        (stack ()))
    (while delim
      (if (or (and (member delim '("$" "$$"))
                   (save-excursion
                     (backward-char (length delim))
                     (tex-parens--math-face-p)))
              (and (not (member delim '("$" "$$")))
                   (tex-parens--open-of-close delim)))
          (push delim stack)
        (let ((other (tex-parens--close-of-open delim)))
          (cl-assert other)
          (if stack
              (progn
                (when tex-parens--debug
                  (unless (equal other (car stack))
                    (message "Mismatched delimiters: %s %s" delim (car stack))))
                (pop stack))
            (forward-char (length delim)))))
      (setq delim (and stack (tex-parens--backward-delim bound))))
    (when stack
      (goto-char start)
      (when tex-parens--debug
        (message "Unmatched delimiters: %s" (car stack))))))

(defun tex-parens-backward-up-list (&optional bound)
  "Move backward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let ((start (point))
        success
        (delim (tex-parens--backward-delim bound))
        (stack ()))
    (while delim
      (cond
       ((or
         (and (member delim '("$" "$$"))
              (save-excursion
                (backward-char (length delim))
                (tex-parens--math-face-p)))
         (and (not (member delim '("$" "$$")))
              (tex-parens--open-of-close delim)))
        (push delim stack))
       (t
        (let ((other (tex-parens--close-of-open delim)))
          (cl-assert other)
          (if stack
              (progn
                (when tex-parens--debug
                  (unless (equal other (car stack))
                    (message "Mismatched delimiters: %s %s" delim (car stack))))
                (pop stack))
            (setq success t))
          ;; (push delim stack))
          
          ;; (if (equal other (car stack))
          ;;     (pop stack)
          ;;   (setq success t))
          )
        ;; (assoc delim tex-parens-pairs)
                                        ; Opening delimiter
        ;; (if (equal (cdr (tex-parens-delim-pair delim)) (car stack))
        ;;     (pop stack)
        ;;   (setq success t))
        )
       )
      (setq delim (and (not success) (tex-parens--backward-delim bound))))
    (unless success
      (goto-char start))))

(defun tex-parens-up-list (&optional bound)
  "Move forward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        success
        (delim (tex-parens--forward-delim bound))
        (stack ()))
    (while delim
      (cond
       ((or
         (and (member delim '("$" "$$"))
              (tex-parens--math-face-p))
         (and (not (member delim '("$" "$$")))
              (tex-parens--close-of-open delim)))
        (push delim stack))
       (t
        (let ((other (tex-parens--open-of-close delim)))
          (cl-assert other)
          (if stack
              (progn
                (when tex-parens--debug
                  (unless (equal other (car stack))
                    (message "Mismatched delimiters: %s %s" delim (car stack))))
                (pop stack))
            (setq success t)))
        ;; (cdr (tex-parens-delim-pair delim))
                                        ; Closing delimiter
        ;; (if (equal (cdr (tex-parens-delim-pair delim)) (car stack))
        ;;     (pop stack)
        ;;   (setq success t))
        )
       )
      (setq delim (and (not success) (tex-parens--forward-delim bound))))
    (unless success
      (goto-char start))))

(defun tex-parens-forward-sexp-1 ()
  "Move forward across one balanced expression (sexp)."
  (interactive)
  (let ((delim-beg (save-excursion
                     (tex-parens--forward-delim)
                     (match-beginning 0)))
        (vanilla (save-excursion
                   (goto-char (or (scan-sexps (point) 1) (buffer-end 1)))
                   (point))))
    (if (and delim-beg
             (> vanilla delim-beg))
        (tex-parens-forward-list)
      (goto-char vanilla))))

(defun tex-parens-backward-sexp ()
  "Move backward across one balanced expression (sexp).
If `backward-sexp' does not take us beyond the ending point of
the previous delimiter, then do that.  Otherwise, do
`tex-parens-backward-list'."
  (interactive)
  (let ((delim-end (save-excursion
                     (when-let ((delim (tex-parens--backward-delim)))
                       (forward-char (length delim))
                       (point))))
        (vanilla (save-excursion
                   (goto-char (or (scan-sexps (point) -1) (buffer-end -1)))
                   (backward-prefix-chars)
                   (point))))
    (if (and delim-end
             (< vanilla delim-end))
        (tex-parens-backward-list)
      (goto-char vanilla))))

(defun tex-parens-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
If `forward-sexp' does not take us past the starting point of the
next delimiter, then do that.  Otherwise, do
`tex-parens-forward-list'.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tex-parens-forward-sexp-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-parens-backward-sexp)
    (setq arg (1+ arg))))

(defun tex-parens-down-list (&optional bound)
  "Move forward into one balanced group.
Search up to BOUND.  Return t if successful, nil otherwise."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        (delim (tex-parens--forward-delim bound))
        success)
    (when (and delim
               (or
                (and (equal delim "$")
                     (tex-parens--math-face-p))
                (and (not (equal delim "$"))
                     (tex-parens--close-of-open delim))))
      (setq success t))
    (unless success
      (goto-char start))
    (when (fboundp 'preview-move-point)
      (preview-move-point))
    success))

(defun tex-parens-delete-pair (&optional bound)
  "Delete a balanced pair of delimiters that follow point.
Push a mark at the end of the contents of the pair.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (when (tex-parens-down-list bound)
    (save-excursion
      (tex-parens-up-list)
      (let ((q (point)))
        (tex-parens--backward-delim)
        (delete-region (point) q)
        (push-mark)))
    (let ((q (point)))
      (tex-parens--backward-delim)
      (delete-region (point) q))))

;;; AWKWARDNESS BEGINS HERE

;; it shouldn't be necessary to define any of the following -- it
;; should suffice to set forward-sexp-function to
;; tex-parens-forward-sexp -- but for some reason, Emacs freezes when
;; I do so.  whatever.  the ad hoc solution works fine

(defun tex-parens-mark-sexp (&optional arg allow-extend)
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
	               (tex-parens-forward-sexp arg)
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
	               (tex-parens-forward-sexp (prefix-numeric-value arg))
              (scan-error
               (user-error (if (equal (cadr error)
                                      "Containing expression ends prematurely")
                               "No sexp to select"
                             (cadr error)))))
	           (point))
	         nil t))))

(defun tex-parens-kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) following point.
With ARG, kill that many sexps after point.
Negative arg -N means kill N sexps before point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (if interactive
      (condition-case _
          (kill-sexp arg nil)
        (scan-error (user-error (if (> arg 0)
                                    "No next sexp"
                                  "No previous sexp"))))
    (let ((opoint (point)))
      (tex-parens-forward-sexp (or arg 1))
      (kill-region opoint (point)))))

(defun tex-parens-backward-kill-sexp (&optional arg interactive)
  "Kill the sexp (balanced expression) preceding point.
With ARG, kill that many sexps before point.
Negative arg -N means kill N sexps after point.
This command assumes point is not in a string or comment.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "p\nd")
  (tex-parens-kill-sexp (- (or arg 1)) interactive))

(defun tex-parens-transpose-sexps-default-function (arg)
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
	            (cons (save-excursion (tex-parens-forward-sexp arg) (point)) (point)))
    ;; Otherwise, we're between sexps.  Take a step back before jumping
    ;; to make sure we'll obey the same precedence no matter which
    ;; direction we're going.
    (funcall (if (> arg 0) #'skip-syntax-backward #'skip-syntax-forward)
             " .")
    (cons (save-excursion (tex-parens-forward-sexp arg) (point))
	         (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			                         (not (zerop (funcall (if (> arg 0)
						                                               #'skip-syntax-forward
						                                             #'skip-syntax-backward)
						                                           ".")))))
		               (point)))))

(defun tex-parens-raise-sexp (&optional n)
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
              (save-excursion (tex-parens-forward-sexp n) (point))))))
    (tex-parens-backward-up-list)
    (delete-region (point) (save-excursion (tex-parens-forward-sexp 1) (point)))
    (save-excursion (insert s))))

(provide 'tex-parens)
;;; tex-parens.el ends here
