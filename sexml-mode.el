;;; sexml-mode.el ---
;;; -*- lexical-binding: t -*-

;; Copyright (C) 2012 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'xmlgen)

(defun symbol->keyword (sym)
  "Converts a symbol to a keyworkd symbol.

 (symbol->keyword 'foo) -> :foo"
  (intern (concat ":" (symbol-name sym))))

(defun xml-attr->xmlgen (attr)
  "Converts an attribute from xml-parse-* format to xmlgen
format.

 (name . value) -> (:name value) "
  (list (symbol->keyword (first attr))
        (rest attr)))

(defun xml-attrs->xmlgen (attrs)
  "Converts attributes from xml-parse-* format to xmlgen
format.

 ((x . v) (y . w)) -> (:x v :y v) "
  (mapcan #'xml-attr->xmlgen attrs))

(defun xml->xmlgen (exp)
  "Converts an libmlx-parse-* expression to xmlgen expression."
  (etypecase exp
    (cons
     (destructuring-bind (tag-name attrs &rest rest)
         exp
       (append
        (list tag-name)
        (xml-attrs->xmlgen attrs)
        (remove
         nil
         (mapcar #'xml->xmlgen rest)))))
    (string
     (unless (null (string-match "[^ \n\t]+" exp))
       (trim-string exp)))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed,
ASCII 10)."
(replace-regexp-in-string "\\`[
\t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)) )

(defun buffer->sexps (&optional buffer-or-name)
  "Converts the given buffer to a list of s-expressions."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (loop with start = (1- (point-min))
          with end = (point-max)
          with content = (buffer-string)

          for ret = (ignore-errors (read-from-string content start))
          until (null ret)
          
          collect (first ret)
          
          do (setf start (1+ (rest ret))))))

(defun sexps->xml (sexps)
  "Converts the given set of s-expressions to XML."
  (loop for sexp in sexps
        for ret = (concat ret
                          (ignore-errors (xmlgen sexp)))

        finally (return ret)))

(defun pretty-print-xml (xml)
  "Indents the given XML."
  (with-temp-buffer
    (sgml-mode)
    (insert xml)
    (sgml-pretty-print (point-min) (point-max))
    (buffer-string)))

(defun buffer-from-sexps-to-xml ()
  "Rewrites the buffer in XML."
  (interactive)
  (let ((new-content
         (sexps->xml (buffer->sexps))))
    (delete-region (point-min) (point-max))
    (insert (pretty-print-xml new-content))))

(defun buffer-from-xml-to-sexps ()
  "Rewrites the buffer in s-expressions."
  (interactive)
  (let ((xml-parse-tree
         (xml-parse-region (point-min) (point-max))))
    (delete-region (point-min) (point-max))
    (insert (pp-to-string (mapcan
                           #'xml->xmlgen
                           xml-parse-tree)))))

(provide 'sexml-mode)

;;; sexml-mode.el ends here
