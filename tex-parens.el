;;; tex-parens.el --- list and sexp navigation for tex modes  -*- lexical-binding: t; -*-

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

;; This package provides sexp and list-based navigation for tex modes,
;; tailored to my use cases
;; (cf. https://github.com/Fuco1/smartparens/issues/1193).
;;
;; Work in progress.  TODO: sexp commands

;;; Code:

(defun tex-parens-setup ()
  (setq
   preview-auto-reveal
   '(eval (preview-arrived-via (key-binding [left])
                               (key-binding [right])
                               #'backward-char #'forward-char #'tex-parens-down))))

(defvar tex-parens-pairs
  '(("(" . ")")
    ("\\Big(" . "\\Big)")
    ("\\left(" . "\\right)")
    ("[" . "]")
    ("\\left[" . "\\right]")
    ("{" . "}")
    ("\\{" . "\\}")
    ("\\left\\{" . "\\right\\}")
    ("\\langle" . "\\rangle")
    ("\\left\\langle" . "\\right\\rangle")
    ("\\lvert" . "\\rvert")
    ("\\left\\lvert" . "\\right\\rvert")
    ("\\lVert" . "\\rVert")
    ("\\left\\lVert" . "\\right\\rVert")
    ("\\left|" . "\\right|")
    ("$" . "$")
    ("``" . "''")))

(defun tex-parens-open ()
  (mapcar #'car tex-parens-pairs))

(defun tex-parens-close ()
  (mapcar #'cdr tex-parens-pairs))

(defun tex-parens-delims ()
  (append (tex-parens-open) (tex-parens-close)))

(defun tex-parens-regexp ()
  (concat (regexp-opt (tex-parens-delims))
          "\\|\\\\begin{[^}]+}\\|\\\\end{[^}]+}"))

(defun tex-parens-reverse-regexp ()
  (concat "}[^{]+{nigeb\\\\\\|}[^{]+{dne\\\\\\|"
          (regexp-opt (mapcar #'reverse (tex-parens-delims)))))

(defun tex-parens-bound-default-forward ()
  (save-excursion
    (min
     (point-max)
     (+ (point) 500))))

(defun tex-parens-bound-default-backward ()
  (save-excursion
    (max
     (point-min)
     (- (point) 500))))

(defun tex-parens-basic-forward (&optional bound)
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-forward)))
  (when (re-search-forward (tex-parens-regexp) bound t)
    (match-string 0)))

(defun tex-parens-basic-backward (&optional bound)
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-backward)))
  (let* ((text (buffer-substring-no-properties bound (point)))
         match result)
    (with-temp-buffer
      (insert (reverse text))
      (goto-char (point-min))
      (setq result (re-search-forward (tex-parens-reverse-regexp) nil t))
      (when result (setq match (match-string 0))))
    (when result
      (backward-char (1- result))
      (reverse match))))

(defun tex-parens-is-open (delim)
  (or
   (cdr (assoc delim tex-parens-pairs))
   (and (stringp delim)
        (string-match "\\\\begin{\\([^}]+\\)}" delim)
        (let ((type (match-string 1 delim)))
          (format "\\end{%s}" type)))))

(defun tex-parens-is-close (delim)
  (or
   (cdr (assoc delim (mapcar (lambda (x) (cons (cdr x) (car x))) tex-parens-pairs)))
   (and (stringp delim)
        (string-match "\\\\end{\\([^}]+\\)}" delim)
        (let ((type (match-string 1 delim)))
          (format "\\begin{%s}" type)))))

(defun tex-parens-face-mathy ()
  (let ((face (plist-get (text-properties-at (point))
                         'face)))
    (or (eq face 'font-latex-math-face)
        (and (listp face)
             (memq 'font-latex-math-face face)))))

(defun tex-parens-forward (&optional bound)
  "Find next TeX sexp. Moves point to end of sexp."
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-forward)))
  (let ((delim (tex-parens-basic-forward bound))
        (stack ()))
    (while delim
      (cond
       ((or
         (and (equal delim "$")
              (tex-parens-face-mathy))
         (and (not (equal delim "$"))
              (tex-parens-is-open delim)))   ; Opening delimiter
        (push delim stack))
       (t
        (let ((other (tex-parens-is-close delim)))
          (cl-assert other)
          (if (equal other (car stack))
              (pop stack)
            (backward-char (length delim))
            ;; (push (cdr (tex-parens-delim-pair delim)) stack)
            ))))
      (setq delim (and stack (tex-parens-basic-forward bound))))))

;; (defun tex-parens-delim-pair (delim)
;;   (or (assoc delim tex-parens-pairs)
;;       (assoc delim (mapcar (lambda (x) (cons (cdr x) (car x))) tex-parens-pairs))))

(defun tex-parens-backward (&optional bound)
  "Find previous TeX sexp. Moves point to start of sexp."
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-backward)))
  (let ((delim (tex-parens-basic-backward bound))
        (stack ()))
    (while delim
      (cond
       ((or
         (and (equal delim "$")
              (save-excursion
                (backward-char)
                (tex-parens-face-mathy)))
         (and (not (equal delim "$"))
              (tex-parens-is-close delim)))
        (push delim stack))
       (
        t
        (let ((other (tex-parens-is-open delim)))
          (cl-assert other)
          (if (equal other (car stack))
              (pop stack)
            (forward-char (length delim))
            ;; (push delim stack)
            ))
        ;; (assoc delim tex-parens-pairs)
                                        ; Opening delimiter
        ;; (if (equal (cdr (tex-parens-delim-pair delim)) (car stack))
        ;;     (pop stack)
        ;;   (forward-char (length delim)))
        )
       )
      (setq delim (and stack (tex-parens-basic-backward bound))))))


(defun tex-parens-backward-up (&optional bound)
  "Find previous TeX sexp. Moves point to start of sexp."
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-backward)))
  (let ((start (point))
        (delim (tex-parens-basic-backward bound))
        (stack ())
        success)
    (while delim
      (cond 
       ((or
         (and (equal delim "$")
              (save-excursion
                (backward-char)
                (tex-parens-face-mathy)))
         (and (not (equal delim "$"))
              (tex-parens-is-close delim)))
        (push delim stack))
       (t
        (let ((other (tex-parens-is-open delim)))
          (cl-assert other)
          (if (equal other (car stack))
              (pop stack)
            (setq success t)))
        ;; (assoc delim tex-parens-pairs)
                                        ; Opening delimiter
        ;; (if (equal (cdr (tex-parens-delim-pair delim)) (car stack))
        ;;     (pop stack)
        ;;   (setq success t))
        )
       )
      (setq delim (and (not success) (tex-parens-basic-backward bound))))
    (unless success
      (goto-char start))))

(defun tex-parens-up (&optional bound)
  "Find previous TeX sexp. Moves point to start of sexp."
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-forward)))
  (let ((start (point))
        (delim (tex-parens-basic-forward bound))
        (stack ())
        success)
    (while delim
      (cond
       ((or
         (and (equal delim "$")
              (tex-parens-face-mathy))
         (and (not (equal delim "$"))
              (tex-parens-is-open delim)))
        (push delim stack))
       (t
        (let ((other (tex-parens-is-close delim)))
          (cl-assert other)
          (if (equal other (car stack))
              (pop stack)
            (setq success t)))
        ;; (cdr (tex-parens-delim-pair delim))
                                        ; Closing delimiter
        ;; (if (equal (cdr (tex-parens-delim-pair delim)) (car stack))
        ;;     (pop stack)
        ;;   (setq success t))
        )
       )
      (setq delim (and (not success) (tex-parens-basic-forward bound))))
    (unless success
      (goto-char start))))

(defun tex-parens-down (&optional bound)
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-forward)))
  (let ((start (point))
        (delim (tex-parens-basic-forward bound))
        success)
    (when (and delim
               (or
                (and (equal delim "$")
                     (tex-parens-face-mathy))
                (and (not (equal delim "$"))
                     (tex-parens-is-open delim))))
      (setq success t))
    (unless success
      (goto-char start))
    (preview-move-point)))

(defun tex-parens-unwrap (&optional bound)
  (interactive)
  (unless bound (setq bound (tex-parens-bound-default-forward)))
  (tex-parens-down)
  (save-excursion
    (tex-parens-up)
    (let ((q (point)))
      (tex-parens-basic-backward)
      (delete-region (point) q)
      (push-mark)))
  (let ((q (point)))
    (tex-parens-basic-backward)
    (delete-region (point) q)))

(provide 'tex-parens)
;;; tex-parens.el ends here
