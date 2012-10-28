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
(require 'xml)
(eval-and-compile

  ;; [4] NameStartChar
  ;; See the definition of word syntax in `xml-syntax-table'.
  (setf xml-name-start-char-re (concat "[![:word:]:_]"))

  ;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
  ;;                 | [#x0300-#x036F] | [#x203F-#x2040]
  (setf xml-name-char-re (concat "[-0-9.![:word:]:_·̀-ͯ‿-⁀]"))

  ;; [5] Name     ::= NameStartChar (NameChar)*
  (setf xml-name-re (concat xml-name-start-char-re xml-name-char-re "*"))

  ;; [6] Names    ::= Name (#x20 Name)*
  (setf xml-names-re (concat xml-name-re "\\(?: " xml-name-re "\\)*"))

  ;; [7] Nmtoken  ::= (NameChar)+
  (setf xml-nmtoken-re (concat xml-name-char-re "+"))

  ;; [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
  (setf xml-nmtokens-re (concat xml-nmtoken-re "\\(?: " xml-name-re "\\)*"))

  ;; [66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'
  (setf xml-char-ref-re  "\\(?:&#[0-9]+;\\|&#x[0-9a-fA-F]+;\\)")

  ;; [68] EntityRef   ::= '&' Name ';'
  (setf xml-entity-ref (concat "&" xml-name-re ";"))

  (setf xml-entity-or-char-ref-re (concat "&\\(?:#\\(x\\)?\\([0-9]+\\)\\|\\("
                                              xml-name-re "\\)\\);"))

  ;; [69] PEReference ::= '%' Name ';'
  (setf xml-pe-reference-re (concat "%\\(" xml-name-re "\\);"))

  ;; [67] Reference   ::= EntityRef | CharRef
  (setf xml-reference-re (concat "\\(?:" xml-entity-ref "\\|" xml-char-ref-re "\\)"))

  ;; [10] AttValue    ::= '"' ([^<&"] | Reference)* '"'
  ;;                    | "'" ([^<&'] | Reference)* "'"
  (setf xml-att-value-re (concat "\\(?:\"\\(?:[^&\"]\\|"
                                     xml-reference-re "\\)*\"\\|"
                                     "'\\(?:[^&']\\|" xml-reference-re
                                     "\\)*'\\)"))

  ;; [56] TokenizedType ::= 'ID'
  ;;     [VC: ID] [VC: One ID / Element Type] [VC: ID Attribute Default]
  ;;                      | 'IDREF'    [VC: IDREF]
  ;;                      | 'IDREFS'   [VC: IDREF]
  ;;                      | 'ENTITY'   [VC: Entity Name]
  ;;                      | 'ENTITIES' [VC: Entity Name]
  ;;                      | 'NMTOKEN'  [VC: Name Token]
  ;;                      | 'NMTOKENS' [VC: Name Token]
  (setf xml-tokenized-type-re (concat "\\(?:ID\\|IDREF\\|IDREFS\\|ENTITY\\|"
                                          "ENTITIES\\|NMTOKEN\\|NMTOKENS\\)"))

  ;; [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
  (setf xml-notation-type-re
    (concat "\\(?:NOTATION\\s-+(\\s-*" xml-name-re
            "\\(?:\\s-*|\\s-*" xml-name-re "\\)*\\s-*)\\)"))

  ;; [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
  ;;       [VC: Enumeration] [VC: No Duplicate Tokens]
  (setf xml-enumeration-re (concat "\\(?:(\\s-*" xml-nmtoken-re
                                       "\\(?:\\s-*|\\s-*" xml-nmtoken-re
                                       "\\)*\\s-+)\\)"))

  ;; [57] EnumeratedType ::= NotationType | Enumeration
  (setf xml-enumerated-type-re (concat "\\(?:" xml-notation-type-re
                                           "\\|" xml-enumeration-re "\\)"))

  ;; [54] AttType    ::= StringType | TokenizedType | EnumeratedType
  ;; [55] StringType ::= 'CDATA'
  (setf xml-att-type-re (concat "\\(?:CDATA\\|" xml-tokenized-type-re
                                    "\\|" xml-notation-type-re
                                    "\\|" xml-enumerated-type-re "\\)"))

  ;; [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
  (setf xml-default-decl-re (concat "\\(?:#REQUIRED\\|#IMPLIED\\|"
                                        "\\(?:#FIXED\\s-+\\)*"
                                        xml-att-value-re "\\)"))

  ;; [53] AttDef      ::= S Name S AttType S DefaultDecl
  (setf xml-att-def-re (concat "\\(?:\\s-*" xml-name-re
                                   "\\s-*" xml-att-type-re
                                   "\\s-*" xml-default-decl-re "\\)"))

  ;; [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
  ;;                   | "'" ([^%&'] | PEReference | Reference)* "'"
  (setf xml-entity-value-re (concat "\\(?:\"\\(?:[^%&\"]\\|"
                                        xml-pe-reference-re
                                        "\\|" xml-reference-re
                                        "\\)*\"\\|'\\(?:[^%&']\\|"
                                        xml-pe-reference-re "\\|"
                                        xml-reference-re "\\)*'\\)")))
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
