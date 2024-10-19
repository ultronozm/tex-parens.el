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
(require 'tex-parens)

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

(provide 'tex-paren-tests)
;;; tex-paren-tests.el ends here
