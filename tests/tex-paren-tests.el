;;; tex-paren-tests.el --- Tests for tex-parens.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>

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

;; Tests for tex-parens.el.

;;; Code:

(require 'ert)
(require 'tex-mode)
(require 'tex-parens)

(defmacro tex-parens-test-with-buffer (contents &rest body)
  "Evaluate BODY within a temporary buffer containing CONTENTS.
The buffer has `tex-parens-mode' enabled so navigation/editing
commands behave as they would in real TeX buffers."  ; doc
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (tex-parens-mode 1)
     (unwind-protect
         (progn ,@body)
       (tex-parens-mode -1))))

(ert-deftest test-tex-parens--reduce-append ()
  "Test the tex-parens--reduce-append function."

  ;; Test with numbers
  (should (equal (tex-parens--reduce-append #'+ '(1 2) '(3 4))
                 '(4 5 5 6)))

  ;; Test with strings
  (should (equal (tex-parens--reduce-append #'concat '("a" "b") '("x" "y"))
                 '("ax" "ay" "bx" "by")))

  ;; Test with empty lists
  (should (equal (tex-parens--reduce-append #'+ '() '(1 2 3))
                 '()))
  (should (equal (tex-parens--reduce-append #'+ '(1 2 3) '())
                 '()))

  ;; Test with custom function
  (should (equal (tex-parens--reduce-append
                  (lambda (x y) (format "%s-%s" x y))
                  '("foo" "bar")
                  '(1 2 3))
                 '("foo-1" "foo-2" "foo-3" "bar-1" "bar-2" "bar-3")))

  ;; Test with lists
  (should (equal (tex-parens--reduce-append #'list '(a b) '(1 2))
                 '((a 1) (a 2) (b 1) (b 2)))))

(declare-function tex-parens--close-of-open "tex-parens.el")

(ert-deftest test-tex-parens--close-of-open ()
  "Test `tex-parens--close-of-open' function."
  ;; Test basic paired delimiters
  (should (equal (tex-parens--close-of-open "(") ")"))
  (should (equal (tex-parens--close-of-open "[") "]"))
  (should (equal (tex-parens--close-of-open "{") "}"))

  ;; Test \begin commands
  (should (equal (tex-parens--close-of-open "\\begin{document}") "\\end{document}"))
  (should (equal (tex-parens--close-of-open "\\begin{figure}") "\\end{figure}"))
  (should (equal (tex-parens--close-of-open "\\begin{tabular}[htbp]") "\\end{tabular}"))

  ;; Test LaTeX commands opening a group
  (should (equal (tex-parens--close-of-open "\\textbf{") "}"))
  (should (equal (tex-parens--close-of-open "\\textit{") "}"))
  (should (equal (tex-parens--close-of-open "\\color[RGB]{255,0,0}{") "}"))

  ;; Test cases that should return nil
  (should (eq (tex-parens--close-of-open "\\end{document}") nil))
  (should (eq (tex-parens--close-of-open ")") nil))
  (should (eq (tex-parens--close-of-open "]") nil))
  (should (eq (tex-parens--close-of-open "}") nil))
  (should (eq (tex-parens--close-of-open "random text") nil)))

(ert-deftest test-tex-parens-forward-and-backward-list ()
  "Forward/backward list navigation respects begin/end pairs."
  (tex-parens-test-with-buffer "\\begin{proof}\nBody\n\\end{proof}"
    (let ((body-pos (progn (search-forward "Body") (match-beginning 0))))
      (goto-char body-pos)
      (let ((end-pos (progn (tex-parens-forward-list) (point))))
        (should (> end-pos body-pos))
        (tex-parens-backward-list)
        (should (= (point) (point-min)))))))

(ert-deftest test-tex-parens-forward-list-equation-env ()
  "Regression test: `tex-parens-forward-list' handles equation envs."
  (tex-parens-test-with-buffer "\\begin{equation}\na=b\n\\end{equation}"
    (let ((body-pos (progn (search-forward "a=b") (match-beginning 0)))
          (env-start (point-min))
          (env-end (save-excursion
                     (search-forward "\\end{equation}")
                     (point))))
      (goto-char body-pos)
      (tex-parens-forward-list)
      (should (= (point) env-end))
      (tex-parens-backward-list)
      (should (= (point) env-start)))))

(ert-deftest test-tex-parens-forward-list-equation-star-env ()
  "Regression test: equation* blocks stay balanced for forward-list."
  (tex-parens-test-with-buffer "\\begin{equation*}\nE=mc^2\n\\end{equation*}"
    (let ((body-pos (progn (search-forward "E=mc^2") (match-beginning 0)))
          (env-end (save-excursion
                     (search-forward "\\end{equation*}")
                     (point))))
      (goto-char body-pos)
      (tex-parens-forward-list)
      (should (= (point) env-end)))))

(ert-deftest test-tex-parens-down-up-list-nested-math ()
  "Down/up list works with nested math delimiters."
  (tex-parens-test-with-buffer "\\left( a + \\left[ b \\right] \\right)"
    (let ((inner-close (save-excursion
                         (goto-char (point-min))
                         (search-forward "\\right]")
                         (point)))
          (outer-close (save-excursion
                         (goto-char (point-min))
                         (search-forward "\\right)")
                         (point))))
      (tex-parens-down-list)
      (should (looking-at " a"))
      (tex-parens-down-list)
      (should (looking-at " b"))
      (tex-parens-up-list)
      (should (= (point) inner-close))
      (tex-parens-up-list)
      (should (= (point) outer-close)))))

(ert-deftest test-tex-parens-forward-backward-sexp-dollar-math ()
  "forward-/backward-sexp treat inline math as a single expression."
  (tex-parens-test-with-buffer "$a+b$ text"
    (tex-parens-forward-sexp)
    (should (eq (char-before) ?$))
    (tex-parens-backward-sexp)
    (should (= (point) (point-min)))))

(ert-deftest test-tex-parens-begin-optional-argument ()
  "Optional arguments on begin/end pairs keep navigation intact."
  (tex-parens-test-with-buffer "\\begin{theorem}[Foo]\nbody\n\\end{theorem}"
    (let ((body-pos (progn (search-forward "body") (match-beginning 0))))
      (goto-char body-pos)
      (let ((end-pos (progn (tex-parens-forward-list) (point))))
        (should (> end-pos body-pos))))))

(ert-deftest test-tex-parens-left-dot-right-bar ()
  "`\left.' and `\right|' delimiters are treated as a pair."
  (tex-parens-test-with-buffer "\\left. f(x) \\right|_{0}^{1}"
    (let ((closing-end (save-excursion
                         (search-forward "\\right|")
                         (point))))
      (tex-parens-forward-sexp)
      (should (= (point) closing-end)))))

(ert-deftest test-tex-parens-left-dot-right-dot ()
  "Invisible \left./\right. delimiters still form a balanced pair."
  (tex-parens-test-with-buffer "\\left. g(x) \\right."
    (let ((end-pos (save-excursion
                     (search-forward "\\right.")
                     (point))))
      (tex-parens-forward-sexp)
      (should (= (point) end-pos)))))

(ert-deftest test-tex-parens-ignores-commented-delimiters ()
  "Delimiters in comments are skipped when searching forward."
  (tex-parens-test-with-buffer "% \\left(ignored\\right)\n\\left(actual\\right)"
    (tex-mode)
    (tex-parens-mode 1)
    (goto-char (point-min))
    (should (equal (tex-parens--forward-delim) "\\left("))
    (should (> (line-number-at-pos) 1))))

(ert-deftest test-tex-parens-delete-pair-removes-delimiters ()
  "delete-pair drops the surrounding Tex delimiters."
  (tex-parens-test-with-buffer "\\left(a+b\\right)"
    (tex-parens-delete-pair)
    (should (equal (buffer-string) "a+b"))))

(ert-deftest test-tex-parens-burp-left-slurps-previous-sexp ()
  "burp-left pulls the preceding sexp into the delimiter pair."
  (tex-parens-test-with-buffer "a \\left( b \\right)"
    (search-forward "\\left")
    (goto-char (match-beginning 0))
    (tex-parens-burp-left)
    (should (equal (buffer-string) "\\left(a  b \\right)"))))

(ert-deftest test-tex-parens-burp-left-barfs-last-sexp ()
  "burp-left pushes the last sexp out of the delimiter pair."
  (tex-parens-test-with-buffer "(a b)"
    (goto-char (point-max))
    (tex-parens-burp-left)
    (should (equal (buffer-string) "(a) b"))))

(ert-deftest test-tex-parens-burp-right-slurps-next-sexp ()
  "burp-right pulls the following sexp into the delimiter pair."
  (tex-parens-test-with-buffer "(a) b"
    (search-forward ")")
    (tex-parens-burp-right)
    (should (equal (buffer-string) "(a b)"))))

(ert-deftest test-tex-parens-adjust-delimiter-size ()
  "Adjusting delimiter size rewrites the modifier pair."
  (tex-parens-test-with-buffer "\\left( x \\right)"
    (tex-parens-adjust-delimiter-size 'increase)
    (should (equal (buffer-string) "\\bigl( x \\bigr)"))
    (tex-parens-adjust-delimiter-size 'decrease)
    (should (equal (buffer-string) "\\left( x \\right)"))))

(provide 'tex-paren-tests)
;;; tex-paren-tests.el ends here
