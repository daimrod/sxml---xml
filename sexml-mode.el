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
  (intern (concat ":" (symbol-name sym))))

(defun libxml-attr->xmlgen (attr)
  (list (symbol->keyword (first attr))
        (rest attr)))

(defun libxml-attrs->xmlgen (attrs)
  (mapcan #'libxml-attr->xmlgen attrs))

(defun libxml->xmlgen (exp)
  (etypecase exp
    (cons
     (destructuring-bind (tag-name attrs &rest rest)
         exp
       (append
        (list tag-name)
        (libxml-attrs->xmlgen attrs)
        (mapcar #'libxml->xmlgen rest))))
    (string exp)))

(defun buffer->sexps (&optional buffer-or-name)
  "Converts the given buffer to a list of s-expressions."
  (let ((buffer-or-name (or buffer-or-name
                            (current-buffer))))
    (with-current-buffer buffer-or-name
      (loop with start = (1- (point-min))
            with end = (point-max)
            with content = (buffer-substring-no-properties (1+ start) end)

            for ret = (ignore-errors (read-from-string content start))
            until (null ret)
            
            collect (first ret)
            
            do (setf start (1+ (rest ret)))))))

(defun sexps->xml (sexps)
  "Converts the given set of s-expressions to XML."
  (loop for sexp in sexps
        for ret = (concat ret
                          (ignore-errors (xmlgen sexp)))

        finally (return ret)))

(defun pretty-print-xml (xml)
  (with-temp-buffer
    (sgml-mode)
    (insert xml)
    (sgml-pretty-print (point-min) (point-max))
    (buffer-string)))

(provide 'sexml-mode)

;;; sexml-mode.el ends here
