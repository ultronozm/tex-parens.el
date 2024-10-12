;;; tex-parens.el --- like lisp.el but for tex  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.6
;; URL: https://github.com/ultronozm/tex-parens.el
;; Package-Requires: ((emacs "27.1"))
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
;;
;; You can activate it via `M-x tex-parens-mode'.  To activate
;; automatically, add the following to your init file:
;;
;; (use-package tex-parens
;;   :hook
;;   (tex-mode . tex-parens-mode)
;;   (TeX-mode . tex-parens-mode))

;;; Code:

(defgroup tex-parens ()
  "Like lisp.el but for tex."
  :group 'tex)

(defun tex-parens--beginning-of-defun ()
  "Move to the beginning of the current defun.
Here `defun' means top-level environment."
  (re-search-backward "^\\\\begin{[^}]+}" nil t))

(defun tex-parens--end-of-defun ()
  "Move to the end of the current defun.
Here `defun' means top-level environment."
  (re-search-forward "^\\\\end{[^}]+}.*\n" nil t))

(defcustom tex-parens-left-right-delimiter-pairs
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

(defcustom tex-parens-solo-delimiters
  '("|" "\\|" "\\vert" "\\Vert")
  "Delimiters that do not come in pairs."
  :type '(repeat string))

(defcustom tex-parens-left-right-modifier-pairs
  '(("\\left" . "\\right")
    ("\\bigl" . "\\bigr")
    ("\\Bigl" . "\\Bigr")
    ("\\biggl" . "\\biggr")
    ("\\Biggl" . "\\Biggr"))
  "Left/right pairs of delimiter modifiers."
  :type '(repeat (cons string string)))

(defcustom tex-parens-solo-modifiers
  '("\\big" "\\Big" "\\bigg" "\\Bigg")
  "Delimiter modifiers that do not come in pairs."
  :type '(repeat string))

(defcustom tex-parens-other-parens
  '(("``" . "''")
    ("{" . "}")
    ("\\(" . "\\)")
    ("\\[" . "\\]")
    ("\\left." . "\\right."))
  "Left/right delimiters not to be combined with modifiers."
  :type '(repeat (cons string string)))

(defcustom tex-parens-max-delim-length 50
  "Maximum length of a delimiter.
This is comfortably larger than `\\biggl\\langle' and
`\\begin{proposition}', for example."
  :type 'integer)

(defun tex-parens--reduce-append (func list1 list2)
  "List consisting of FUNC applied to pairs from LIST1 and LIST2."
  (seq-reduce #'append
              (mapcar (lambda (item1)
                        (mapcar (lambda (item2)
                                  (funcall func item1 item2))
                                list2))
                      list1)
              nil))

(defun tex-parens--generate-pairs ()
  "Generate list of left/right pairs of delimiters.
With the exception of the math delimiters `$' and `$$', we only
form delimiters which are visibly `left'/`opening' or
`right'/`closing'."
  (let ((unambiguous-parens tex-parens-left-right-delimiter-pairs)
        (ambiguous-parens tex-parens-solo-delimiters)
        (unambiguous-modifiers tex-parens-left-right-modifier-pairs)
        (ambiguous-modifiers tex-parens-solo-modifiers))
    (append
     tex-parens-other-parens
     '(("$" . "$")
       ("$$" . "$$"))
     unambiguous-parens
     (tex-parens--reduce-append
      (lambda (up um)
        (cons (concat (car um) (car up))
              (concat (cdr um) (cdr up))))
      unambiguous-parens unambiguous-modifiers)
     (tex-parens--reduce-append
      (lambda (up m)
        (cons (concat m (car up))
              (concat m (cdr up))))
      unambiguous-parens ambiguous-modifiers)
     (tex-parens--reduce-append
      (lambda (p um)
        (cons (concat (car um) p)
              (concat (cdr um) p)))
      ambiguous-parens unambiguous-modifiers))))

;; The significance of the "+" regexps is that for some (but not all)
;; purposes, it is convenient to regard the optional arguments
;; following a \\begin{...} delimiter as being part of that delimiter.
;; This is so for slurp/barf commands, but not for other navigation
;; and editing commands, which one might wish to apply directly to the
;; brackets surrounding the optional argument.
(defvar tex-parens--pairs nil)
(defvar tex-parens--pairs-swap nil)
(defvar tex-parens--delims nil)
(defvar tex-parens--regexp nil)
(defvar tex-parens--regexp+ nil)
(defvar tex-parens--regexp-open nil)
(defvar tex-parens--regexp-open+ nil)
(defvar tex-parens--regexp-close nil)
(defvar tex-parens--regexp-reverse nil)
(defvar tex-parens--regexp-reverse+ nil)

(defvar tex-parens--saved-beginning-of-defun-function nil)
(defvar tex-parens--saved-end-of-defun-function nil)

(defun tex-parens-setup ()
  "Set up tex-parens.  Intended as a hook for `tex-mode' or `TeX-mode'."

  ;; If AUCTeX 14.0.5+ is installed, then we make some of the
  ;; navigation commands automatically open previews and folds.
  (when (boundp 'preview-auto-reveal-commands)
    (dolist (func '(tex-parens-down-list
                    tex-parens-backward-down-list))
      (add-to-list 'preview-auto-reveal-commands func)))

  (when (boundp 'TeX-fold-auto-reveal-commands)
    (dolist (func '(tex-parens-down-list
                    tex-parens-backward-down-list
                    tex-parens-up-list
                    tex-parens-backward-up-list
                    tex-parens-forward-list
                    tex-parens-backward-list
                    tex-parens-forward-sexp
                    tex-parens-backward-sexp))
      (add-to-list 'TeX-fold-auto-reveal-commands func)))

  (setq-local tex-parens--saved-beginning-of-defun-function
              beginning-of-defun-function)
  (setq-local beginning-of-defun-function #'tex-parens--beginning-of-defun)
  (setq-local tex-parens--saved-end-of-defun-function
              end-of-defun-function)
  (setq-local end-of-defun-function #'tex-parens--end-of-defun)
  (setq tex-parens--pairs (tex-parens--generate-pairs))
  (setq tex-parens--pairs-swap
        (mapcar (lambda (x) (cons (cdr x) (car x))) tex-parens--pairs))
  (setq tex-parens--delims (append (mapcar #'car tex-parens--pairs)
                                   (mapcar #'cdr tex-parens--pairs)))

  (setq tex-parens--regexp
        (concat
         ;; "\\\\begin{[^}]+}\\[[^]]+\\]" "\\|"
         "\\\\begin{[^}]+}" "\\|"
         "\\\\end{[^}]+}" "\\|"
         "\\\\[a-zA-Z]+\\[[^]]+\\]{" "\\|"
         "\\\\[a-zA-Z]+{" "\\|"
         (regexp-opt tex-parens--delims)))
  (setq tex-parens--regexp+
        (concat
         "\\\\begin{[^}]+}\\[[^]]+\\]" "\\|"
         tex-parens--regexp))
  (setq tex-parens--regexp-open
        (concat (regexp-opt (mapcar #'car tex-parens--pairs))
                "\\|\\\\begin{[^}]+}"
                "\\|\\\\[a-zA-Z]+\\[[^]]+\\]{"
                "\\|\\\\[a-zA-Z]+{"))
  (setq tex-parens--regexp-open+
        (concat
         "\\\\begin{[^}]+}\\[[^]]+\\]\\|"
         tex-parens--regexp-open))
  (setq tex-parens--regexp-close
        (concat (regexp-opt (mapcar #'cdr tex-parens--pairs))
                "\\|\\\\end{[^}]+}"))
  (setq tex-parens--regexp-reverse
        (concat
         "}[^}]+{nigeb\\\\\\|"
         "}[^}]+{dne\\\\\\|"
         "{[a-zA-Z]+\\\\\\|"
         "{\\][^]]+\\[[a-zA-Z]+\\\\\\|"
         (regexp-opt (mapcar #'reverse tex-parens--delims))))
  (setq tex-parens--regexp-reverse+
        (concat "\\][^[]+\\[}[^{]+{nigeb\\\\\\|"
                tex-parens--regexp-reverse))

  ;; It would be natural to uncomment the following line, but I had
  ;; problems with it at some point, perhaps related to the fact that
  ;; other modes use the built-in forward-sexp in various ways.  This
  ;; means that we need to define a few more boilerplate functions and
  ;; bind a few more keys, but seems like the stable approach.

  ;; (setq-local forward-sexp-function #'tex-parens-forward-sexp)
  )

;;;###autoload
(define-minor-mode tex-parens-mode
  "Toggle tex-parens mode.
Tex Parens mode is a minor mode in which lisp/sexp/defun-based commands
are adapted to tex environments and math delimiters.  The affected
commands include, for instance, `forward-sexp', `forward-list' and
`beginning-of-defun'."
  :lighter nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap forward-sexp] #'tex-parens-forward-sexp)
            (define-key map [remap backward-sexp] #'tex-parens-backward-sexp)
            (define-key map [remap forward-list] #'tex-parens-forward-list)
            (define-key map [remap backward-list] #'tex-parens-backward-list)
            (define-key map [remap backward-up-list]
                        #'tex-parens-backward-up-list)
            (define-key map [remap up-list] #'tex-parens-up-list)
            (define-key map [remap down-list] #'tex-parens-down-list)
            (define-key map [remap delete-pair] #'tex-parens-delete-pair)
            (define-key map [remap mark-sexp] #'tex-parens-mark-sexp)
            (define-key map [remap kill-sexp] #'tex-parens-kill-sexp)
            (define-key map [remap backward-kill-sexp]
                        #'tex-parens-backward-kill-sexp)
            (define-key map [remap transpose-sexps]
                        #'tex-parens-transpose-sexps)
            (define-key map [remap raise-sexp] #'tex-parens-raise-sexp)
            map)
  (cond
   (tex-parens-mode
    (tex-parens-setup))
   (t
    (setq-local beginning-of-defun-function
                tex-parens--saved-beginning-of-defun-function)
    (setq-local end-of-defun-function
                tex-parens--saved-end-of-defun-function))))

(defcustom tex-parens-search-limit 10000
  "How far to search for a delimiter, in either direction.
This should exceed the length, in characters, of the longest
theorem-like environments to which you care about applying the
list and sexp-based navigation commands.  Longer environments
typically occur at the top level and are best navigated using the
defun-based commands."
  :type 'integer)

(defun tex-parens--math-face ()
  "Return the number of math face modifiers at point.
0 means no math face."
  (let ((math-faces '(tex-math font-latex-math-face))
        (face (plist-get (text-properties-at (point))
                         'face)))
    (cond
     ((memq face math-faces)
      1)
     ((listp face)
      (let ((total 0))
        (dolist (f face)
          (when (memq f math-faces)
            (setq total (1+ total))))
        total))
     (t 0))))

(defcustom tex-parens-ignore-comments t
  "Whether to ignore comments when searching for delimiters."
  :type 'boolean)

(defun tex-parens--ignore (str begin end)
  "Check if STR should be ignored.
STR is the string matched by the search, while BEGIN and END delimit the
match.  If `tex-parens-ignore-comments' is non-nil, then ignore
comments; these are detected via `font-lock-comment-face'.  If STR is a
double prime in math mode, then ignore it.  If STR is a dollar delimiter
that does not demarcate math mode, then ignore it."
  (or (and tex-parens-ignore-comments
           (save-excursion (goto-char begin)
                           (nth 4 (syntax-ppss))))
      (and (equal str "''")
           (save-excursion (goto-char begin)
                           (> (tex-parens--math-face) 0)))
      (and (member str '("$" "$$"))
           (equal (save-excursion (goto-char (1- begin))
                                  (tex-parens--math-face))
                  (save-excursion (goto-char end)
                                  (tex-parens--math-face))))))

(defun tex-parens--search-forward (regexp bound)
  "Search forward for REGEXP up to BOUND.
Ignore matches that should be ignored.  Return the first match string
found, or nil if none is found."
  (let (success done)
    (while (not done)
      (if (re-search-forward regexp bound t)
          (when (not (tex-parens--ignore (match-string 0)
                                         (match-beginning 0)
                                         (match-end 0)))
            (setq done t
                  success t))
        (setq done t)))
    (when success
      (match-string 0))))

(defun tex-parens--search-backward (regexp regexp-reverse bound)
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
                              (- (point) tex-parens-max-delim-length))
                         (point))))
              (with-temp-buffer
                (insert (reverse text))
                (goto-char (point-min))
                (re-search-forward regexp-reverse nil t)
                (setq match (reverse (match-string 0))))
              (backward-char (length match))
              (when (not (tex-parens--ignore match
                                     (point)
                                     (+ (point) (length match))))
                (setq done t)
                (setq success t))))
        ;; didn't find anything, so we failed
        (setq done t)))
    (when success
      match)))

(defun tex-parens--forward-bound ()
  "Return the default bound for forward search."
  (save-excursion
    (min (point-max)
         (+ (point) tex-parens-search-limit))))

(defun tex-parens--backward-bound ()
  "Return the default bound for backward search."
  (save-excursion
    (max (point-min)
         (- (point) tex-parens-search-limit))))

(defun tex-parens--forward-delim (&optional bound)
  "Search for the next delimiter up to BOUND.
Return the delimiter found, or nil if none is found."
  (unless bound (setq bound (tex-parens--forward-bound)))
  (tex-parens--search-forward tex-parens--regexp bound))

(defun tex-parens--backward-delim (&optional bound)
  "Search for the previous delimiter up to BOUND.
Return the delimiter found, or nil if none is found."
  (unless bound (setq bound (tex-parens--backward-bound)))
  (tex-parens--search-backward
   tex-parens--regexp tex-parens--regexp-reverse bound))

(defun tex-parens--close-of-open (delim)
  "Check if DELIM is opening, return the corresponding closing.
If DELIM is an opening delimiter, return the corresponding closing
delimiter.  Otherwise, return nil."
  (or
   (cdr (assoc delim tex-parens--pairs))
   (and
    (stringp delim)
    (or
     (and (or
           (string-match "\\\\begin{\\([^}]+\\)}\\[[^]]+\\]" delim)
           (string-match "\\\\begin{\\([^}]+\\)}" delim))
          (let ((type (match-string 1 delim)))
            (format "\\end{%s}" type)))
     (unless (string-match "\\\\end{\\([^}]+\\)}" delim)
       (and (string-match "\\\\[a-zA-Z]+\\[[^]]+\\]{\\|\\\\[a-zA-Z]+{" delim)
            "}"))))))

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

(defvar tex-parens--debug nil)

(defun tex-parens--forward-search-found-open (delim)
  "Check if DELIM is an opening delimiter found by forward search."
  (cond
   ((member delim '("$" "$$"))
    (>
     (tex-parens--math-face)
     (save-excursion
       (backward-char (1+ (length delim)))
       (tex-parens--math-face))))
   (t
    (tex-parens--close-of-open delim))))

(defun tex-parens--backward-search-found-close (delim)
  "Check if DELIM is a closing delimiter found by backward search."
  (cond
   ((member delim '("$" "$$"))
    (>
     (save-excursion
       (backward-char 1)
       (tex-parens--math-face))
     (save-excursion
       (forward-char (length delim))
       (tex-parens--math-face))))
   (t
    (tex-parens--open-of-close delim))))

(defun tex-parens--check-match (delim other stack-top)
  "Internal function used for debugging.
Check that OTHER is non-nil.  This should always be the case.  Then, if
debugging is enabled, check whether STACK-TOP and DELIM coincide.
Sometimes this is intentional (e.g., when `\\right.'  terminates
`\\left{'), so we do not treat it as an error."
  (when tex-parens--debug
    (unless (equal other stack-top)
      (message "Mismatched delimiters: %s %s" stack-top delim))))

(defun tex-parens-forward-list (&optional arg)
  "Move forward across one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward across N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tex-parens--forward-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-parens--backward-list-1)
    (setq arg (1+ arg))))

(defun tex-parens-backward-list (&optional arg)
  "Move backward across one balanced group.
With ARG, do it that many times.  Negative
arg -N means move forward across N balanced groups."
  (interactive "^p")
  (tex-parens-forward-list (- (or arg 1))))

(defun tex-parens--forward-list-1 (&optional bound)
  "Move forward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        (delim (tex-parens--forward-delim bound))
        (stack ())
        success)
    (while delim
      (if (tex-parens--forward-search-found-open delim)
          (push delim stack)
        (if stack
            (progn
              (tex-parens--check-match delim (tex-parens--open-of-close delim)
                                       (car stack))
              (pop stack)
              (unless stack
                (setq success t)))
          (backward-char (length delim))
          (setq success t)))
      (setq delim (and (not success) (tex-parens--forward-delim bound))))
    (unless success
      (goto-char start)
      (when tex-parens--debug
        (message "Unmatched delimiters: %s" (car stack))))))

(defun tex-parens--backward-list-1 (&optional bound)
  "Move backward across one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let ((start (point))
        (delim (tex-parens--backward-delim bound))
        (stack ())
        success)
    (while delim
      (if (tex-parens--backward-search-found-close delim)
          (push delim stack)
        (if stack
            (progn
              (tex-parens--check-match delim (tex-parens--close-of-open delim)
                                       (car stack))
              (pop stack)
              (unless stack
                (setq success t)))
          (forward-char (length delim))
          (setq success t)))
      (setq delim (and (not success) (tex-parens--backward-delim bound))))
    (unless success
      (goto-char start)
      (when tex-parens--debug
        (message "Unmatched delimiters: %s" (car stack))))))

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
    (tex-parens--forward-sexp-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-parens--backward-sexp-1)
    (setq arg (1+ arg))))

(defun tex-parens-backward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
If `forward-sexp' does not take us past the starting point of the
next delimiter, then do that.  Otherwise, do
`tex-parens-forward-list'.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions."
  (interactive "^p")
  (tex-parens-forward-sexp (- (or arg 1))))

(defun tex-parens--forward-sexp-1 ()
  "Move forward across one balanced expression (sexp).
Helper function for `tex-parens-forward-sexp'."
  (let ((delim-beg (save-excursion
                     (when (tex-parens--forward-delim)
                       (match-beginning 0))))
        (vanilla
         (condition-case _
             (save-excursion
               (goto-char (or (scan-sexps (point) 1) (buffer-end 1)))
               (point))
           (scan-error nil))))
    (if (or (not vanilla)
            (and delim-beg
                 (> vanilla delim-beg)))
        (tex-parens--forward-list-1)
      (goto-char vanilla))))

(defun tex-parens--backward-sexp-1 ()
  "Move backward across one balanced expression (sexp).
If `backward-sexp' does not take us beyond the ending point of
the previous delimiter, then do that.  Otherwise, do
`tex-parens-backward-list'."
  (interactive)
  (let ((delim-end (save-excursion
                     (when-let ((delim (tex-parens--backward-delim)))
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
        (tex-parens--backward-list-1)
      (goto-char vanilla))))

(defun tex-parens-up-list (&optional arg)
  "Move forward out of one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward out of N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tex-parens--up-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-parens--backward-up-list-1)
    (setq arg (1+ arg))))

(defun tex-parens-backward-up-list (&optional arg)
  "Move backward out of one balanced group.
With ARG, do it that many times.  Negative arg -N means move
forward out of N balanced groups."
  (interactive "^p")
  (tex-parens-up-list (- (or arg 1))))

(defun tex-parens--up-list-1 (&optional bound)
  "Move forward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        success
        (delim (tex-parens--forward-delim bound))
        (stack ()))
    (while delim
      (if (tex-parens--forward-search-found-open delim)
          (push delim stack)
        (if stack
            (progn
              (tex-parens--check-match delim (tex-parens--open-of-close delim)
                                       (car stack))
              (pop stack))
          (setq success t)))
      (setq delim (and (not success) (tex-parens--forward-delim bound))))
    (unless success
      (goto-char start))))

(defun tex-parens--backward-up-list-1 (&optional bound)
  "Move backward out of one balanced group.
Search up to BOUND."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let ((start (point))
        success
        (delim (tex-parens--backward-delim bound))
        (stack ()))
    (while delim
      (if (tex-parens--backward-search-found-close delim)
          (push delim stack)
        (if stack
            (progn
              (tex-parens--check-match delim (tex-parens--close-of-open delim)
                                       (car stack))
              (pop stack))
          (setq success t)))
      (setq delim (and (not success) (tex-parens--backward-delim bound))))
    (unless success
      (goto-char start))))

(defun tex-parens-down-list (&optional arg)
  "Move forward into one balanced group.
With ARG, do it that many times.  Negative arg -N means move
backward into N balanced groups."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (tex-parens--down-list-1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tex-parens--backward-down-list-1)
    (setq arg (1+ arg))))

(defun tex-parens-backward-down-list (&optional arg)
  "Move backward into one balanced group.
With ARG, do it that many times.  Negative arg -N means move
forward into N balanced groups."
  (interactive "^p")
  (tex-parens-down-list (- (or arg 1))))

(defun tex-parens--down-list-1 (&optional bound)
  "Move forward into one balanced group.
Search up to BOUND.  Return t if successful, nil otherwise."
  (interactive)
  (unless bound (setq bound (tex-parens--forward-bound)))
  (let ((start (point))
        (delim (tex-parens--forward-delim bound))
        success)
    (when (and delim
               (tex-parens--forward-search-found-open delim))
      (setq success t))
    (unless success
      (goto-char start))
    success))

(defun tex-parens--backward-down-list-1 (&optional bound)
  "Move backward into one balanced group.
Search up to BOUND.  Return t if successful, nil otherwise."
  (interactive)
  (unless bound (setq bound (tex-parens--backward-bound)))
  (let ((start (point))
        (delim (tex-parens--backward-delim bound))
        success)
    (when (and delim
               (tex-parens--backward-search-found-close delim))
      (setq success t))
    (unless success
      (goto-char start))
    success))

(defun tex-parens-delete-pair ()
  "Delete a balanced pair of delimiters that follow point.
Push a mark at the end of the contents of the pair."
  (interactive)
  (when (tex-parens--down-list-1)
    (save-excursion
      (tex-parens-up-list)
      (let ((q (point)))
        (tex-parens--backward-delim)
        (delete-region (point) q)
        (push-mark)))
    (let ((q (point)))
      (tex-parens--backward-delim)
      (delete-region (point) q))))


;;; The following is adapted from lisp.el and simple.el

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
          (tex-parens-kill-sexp arg nil)
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

(defun tex-parens-transpose-sexps-function (arg)
  "Default method to locate a pair of points for
`tex-parens-transpose-sexps'.  ARG is as in the docstring for
`tex-parens-transpose-sexps'."
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
             (cons (save-excursion (tex-parens-forward-sexp arg) (point))
                   (point)))
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

(defun tex-parens-transpose-sexps (arg &optional interactive)
  "Like \\[transpose-chars] (`transpose-chars'), but applies to sexps.
Unlike `transpose-words', point must be between the two sexps and not
in the middle of a sexp to be transposed.
With non-zero prefix arg ARG, effect is to take the sexp before point
and drag it forward past ARG other sexps (backward if ARG is negative).
If ARG is zero, the sexps ending at or after point and at or after mark
are interchanged.
If INTERACTIVE is non-nil, as it is interactively,
report errors as appropriate for this kind of usage."
  (interactive "*p\nd")
  (if interactive
      (condition-case nil
          (tex-parens-transpose-sexps arg nil)
        (scan-error (user-error "Not between two complete sexps")))
    (transpose-subr #'tex-parens-transpose-sexps-function arg 'special)))

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

;;; BURP

(defun tex-parens--slurp-left ()
  "Slurp the next sexp into the current one, to the left."
  (when-let ((pos (point))
             (match (when (looking-at tex-parens--regexp+) (match-string 0))))
    (delete-region (point) (+ (point) (length match)))
    (condition-case nil
        (progn
          (tex-parens-backward-sexp)
          (insert match)
          (backward-char (length match)))
      (error
       (goto-char pos)
       (insert match)
       (backward-char (length match))))))

(defun tex-parens--barf-left ()
  "Barf the next sexp out of the current one, to the right."
  (when-let* ((pos (point))
              (bound (max (point-min)
                          (- (point) tex-parens-max-delim-length)))
              (text (buffer-substring bound pos))
              (reversed-text (reverse text))
              (reverse-match
               (with-temp-buffer
                 (insert reversed-text)
                 (goto-char (point-min))
                 (when (looking-at tex-parens--regexp-reverse+)
                   (match-string 0))))
              (match (reverse reverse-match)))
    (backward-char (length match))
    (progn
      (tex-parens-backward-sexp)
      (let ((q
             (save-excursion
               (tex-parens-forward-sexp)
               (point))))
        (tex-parens-backward-sexp)
        (when (not (equal q
                          (save-excursion
                            (tex-parens-forward-sexp)
                            (point))))
          (tex-parens-forward-sexp))))
    (insert match)
    (save-excursion
      (goto-char pos)
      (delete-char (length match)))))

(defun tex-parens-burp-left ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((and
     (not (looking-back tex-parens--regexp-open+
                        (max (point-min) (- (point)
                                            tex-parens-max-delim-length))))
     (looking-back tex-parens--regexp-close
                   (max (point-min) (- (point)
                                       tex-parens-max-delim-length))))
    (tex-parens--barf-left))
   ((and
     (not (looking-at tex-parens--regexp-close))
     (looking-at tex-parens--regexp-open+))
    (tex-parens--slurp-left))
   (t
    (call-interactively #'self-insert-command))))

(defun tex-parens--barf-right ()
  "Barf the next sexp out of the current one, to the right."
  (let ((pos (point))
        (match (when (looking-at tex-parens--regexp+) (match-string 0))))
    (forward-char (length match))
    (progn
      (tex-parens-forward-sexp)
      (let ((q
             (save-excursion
               (tex-parens-backward-sexp)
               (point))))
        (tex-parens-forward-sexp)
        (when (not (equal q
                          (save-excursion
                            (tex-parens-backward-sexp)
                            (point))))
          (tex-parens-backward-sexp))))
    (insert match)
    (save-excursion
      (goto-char pos)
      (delete-char (length match)))
    (backward-char (length match))))

(defun tex-parens--slurp-right ()
  "Slurp the next sexp into the current one, to the right."
  (when-let* ((pos (point))
              (bound (max (point-min)
                          (- (point) tex-parens-max-delim-length)))
              (text (buffer-substring bound pos))
              (reversed-text (reverse text))
              (reverse-match
               (with-temp-buffer
                 (insert reversed-text)
                 (goto-char (point-min))
                 (when (looking-at tex-parens--regexp-reverse+)
                   (match-string 0))))
              (match (reverse reverse-match)))
    (condition-case nil
        (progn
          (tex-parens-forward-sexp)
          (insert match)
          (save-excursion
            (goto-char pos)
            (backward-char (length match))
            (delete-char (length match))))
      (error nil))))

(defun tex-parens-burp-right ()
  "Slurp or barf to the right.
If the point is before a list, slurp the next sexp into the list.
If the point is after a list, barf the last sexp out of the list.
If the point is before a quote, slurp the next sexp into the quote.
If the point is after a quote, barf the last sexp out of the quote.
Otherwise, call `self-insert-command'."
  (interactive)
  (cond
   ((and
     (not (looking-back tex-parens--regexp-open+
                        (max (point-min) (- (point)
                                            tex-parens-max-delim-length))))
     (looking-back tex-parens--regexp-close
                   (max (point-min) (- (point)
                                       tex-parens-max-delim-length))))
    (tex-parens--slurp-right))
   ((and
     (not (looking-at tex-parens--regexp-close))
     (looking-at tex-parens--regexp-open+))
    (tex-parens--barf-right))
   (t
    (call-interactively #'self-insert-command))))

;;; Miscellaneous

(defun tex-parens-mark-inner ()
  "Mark the innermost balanced group around point."
  (interactive)
  (tex-parens-backward-up-list)
  (tex-parens-down-list)
  (set-mark (point))
  (tex-parens-up-list)
  (tex-parens-backward-down-list))

(defun tex-parens-beginning-of-list ()
  "Move to the beginning of the current balanced group."
  (interactive)
  (let ((last (point)))
    (tex-parens-backward-sexp)
    (while (< (point) last)
      (setq last (point))
      (tex-parens-backward-sexp))))

(defun tex-parens-end-of-list ()
  "Move to the end of the current balanced group."
  (interactive)
  (let ((last (point)))
    (tex-parens-forward-sexp)
    (while (> (point) last)
      (setq last (point))
      (tex-parens-forward-sexp))))

(defun tex-parens-kill-to-end-of-list ()
  "Kill text between point and end of current list."
  (interactive)
  (let ((end (save-excursion (tex-parens-end-of-list) (point))))
    (kill-region (point) end)))

(defun tex-parens-kill-to-beginning-of-list ()
  "Kill text between point and beginning of current list."
  (interactive)
  (let ((beginning (save-excursion (tex-parens-beginning-of-list) (point))))
    (kill-region beginning (point))))

;;; Avy integration

(defcustom tex-parens-avy-regexp
  (rx (or
       (seq (= 2 anything) "$$")
       (seq (= 2 anything) "\\(")
       (seq (= 2 anything) "\\[")
       (seq anything " $")
       (seq (= 2 anything)
            "\n"
            (zero-or-more space)
            "\\begin{"
            (or "eq" "ali" "multline" "gath"))))
  "Regular expression for `tex-parens-avy-jump-to-math'.
This regexp matches the start of various math environments, keeping a
couple of characters before the primary match to leave space for Avy."
  :type 'regexp)

(defun tex-parens-avy-jump-to-math ()
  "Jump inside a math expression using Avy.
This function uses `tex-parens-avy-regexp' to identify potential math
expressions, then jumps to the selected one and moves point inside the
expression."
  (interactive)
  (if (fboundp 'avy-jump)
      (avy-jump tex-parens-avy-regexp
                :action (lambda (pos)
                          (goto-char pos)
                          (forward-char 2)
                          (tex-parens-down-list)
                          ;; For preview-auto-reveal:
                          (setq this-command #'tex-parens-down-list)))
    (user-error "Avy is not available.  Please install and load it to use this function")))

(defun tex-parens-avy-copy-math ()
  "Copy a math expression selected using Avy.
This function uses `tex-parens-avy-regexp' to identify potential math
expressions, then copies the selected one to the kill ring."
  (interactive)
  (if (fboundp 'avy-jump)
      (avy-jump tex-parens-avy-regexp
                :action (lambda (pos)
                          (let ((beg (+ pos 2))
                                (end (save-excursion
                                       (goto-char (+ pos 2))
                                       (tex-parens-forward-list)
                                       (point))))
                            (copy-region-as-kill beg end)
                            (message "Math expression copied"))))
    (user-error "Avy is not available.  Please install and load it to use this function")))

(provide 'tex-parens)
;;; tex-parens.el ends here
