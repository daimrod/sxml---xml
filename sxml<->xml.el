;;; sxml<->xml.el ---
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
(require 'xml)
(require 'cl)

;;; Redefined to make it add a newline after each tag.
(defun sgml-pretty-print (beg end)
  "Simple-minded pretty printer for SGML.
Re-indents the code and inserts newlines between BEG and END. You
might want to turn on `auto-fill-mode' to get better results."
  (interactive "r")
  (save-excursion
    (if (< beg end)
        (goto-char beg)
      (goto-char end)
      (setq end beg)
      (setq beg (point)))
    ;; Don't use narrowing because it screws up auto-indent.
    (setq end (copy-marker end t))
    (with-syntax-table sgml-tag-syntax-table
      (while (re-search-forward "<" end t)
        (goto-char (match-beginning 0))
        (unless (or ;;(looking-at "</")
                 (progn (skip-chars-backward " \t") (bolp)))
          (reindent-then-newline-and-indent))
        (forward-sexp 1)
        (reindent-then-newline-and-indent)))))

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
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

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

(defvar *sxml<->xml-cur* nil
  "Buffer local variable used to memorize the orignal buffer.")
(make-local-variable '*sxml<->xml-cur*)

(defun edit-xml ()
  (interactive)
  (destructuring-bind (dtd &rest xml)
      (xml-parse-region (point-min) (point-max) nil t)
    (setf *sxml<->xml-cur* (current-buffer))
    (switch-to-buffer "*SXML<->XML*")
    (delete-region (point-min) (point-max))
    (emacs-lisp-mode)
    (local-set-key (kbd "C-c '") 'write-xml)
    (insert (pp-to-string
             (mapcan #'xml->xmlgen
                     xml)))))

(defun write-xml ()
  (interactive)
  (let ((new-content
         (sexps->xml (buffer->sexps))))
    (local-unset-key (kbd "C-c '"))
    (switch-to-buffer *sxml<->xml-cur*)
    (delete-region (point-min) (point-max))
    (insert (pretty-print-xml new-content))))

(require 'nxml-mode)
(define-key nxml-mode-map (kbd "C-c '") 'edit-xml)

(provide 'sxml<->xml)

;;; sxml<->xml.el ends here
