;;; sql-smie.el --- SMIE support for SQL-mode        -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 0

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

;; This package provides SMIE support for SQL-mode.
;;
;; It is currently quite inferior to `sql-indent' at indenting SQL code because
;; it only handles a tiny subset of the SQL language, so don't bother trying it
;; out for that purpose unless you're interested in improving it.
;;
;; To make use of it, you can do something like:
;;
;;     (add-hook 'sql-mode-hook #'sql-smie-enable)
;;
;; It can be used together with `sql-indent', where `sql-indent' takes care
;; of indenting and `sql-smie' is used for navigation, auto-fill, blink-paren,
;; etc...

;;; Code:

(require 'smie)

(defconst sql-smie-grammar
  ;; FIXME:
  ;; - Terribly incomplete.
  (smie-prec2->grammar
   (smie-bnf->prec2
    ;; Note: To improve this, https://www.h2database.com/html/grammar.html
    ;; could be useful.
    '((id)
      (insts ("BEGIN" insts "END")
             (cmd)
             (foo)
             (insts ";" insts)
             ("FOR" range "LOOP" insts "END")
             ("IF" cond-then-insts "END")
             ("IF" cond-then-insts "ELSE" insts "END")
             ("IF" cond-then-insts "ELSIF" cond-then-insts "ELSE" insts "END"))
      (cond-then-insts (cond "THEN" insts))
      (cond (exp))
      (range (id "IN" cmd))
      (cmd ("SELECT" after-select )
           ("INSERT INTO" cmd)
           )
      (after-select (exp "FROM" arg-of-from)
                    (after-select "WHERE" arg-of-where)
                    (after-select "foo BY" after-select))
      (arg-of-where (arg-of-where "AND" arg-of-where)
                    (arg-of-where "OR" arg-of-where)(exp))
      (arg-of-from  (arg-of-from "foo JOIN" arg-of-from)
                    (arg-of-from "ON" arg-of-from))
      ;; (select-exp ("*") (exp) (exp "AS" column-alias))
      ;; (table-exp (id) (table-exp "foo JOIN" table-exp)
      ;;            (table-exp "ON" exp))
      (exp (id "IN" exp) (exp "=" exp)
           ;; FIXME: The "CASE" rules look plain wrong!
           ("CASE" exp "WHEN" cond-then-insts "ELSE" exp "END")
           ("CASE" exp "WHEN" cond-then-insts "END")
           (exp "," exp) (exp "||" exp))
      )
    '((assoc ";"))
    '((assoc "ELSE"))
    '((assoc "IN") (assoc ",") (assoc "||") (assoc "="))
    '((assoc "FROM" "WHERE" "foo BY") (assoc "OR") (assoc "AND"))
    '((assoc "foo JOIN") (assoc "ON")))))

(defconst sql-smie--token-pairs
  '(("END" ("IF" "LOOP"))
    ;; FIXME: Reality is more complex than that:
    ;;     [ { { LEFT | RIGHT } [ OUTER ] | [ INNER ] | CROSS | NATURAL }
    ;;     JOIN tableExpression [ joinSpecification ] ]
    (("NATURAL" "INNER" "OUTER" "LEFT" "RIGHT") "JOIN" "foo JOIN")
    (("GROUP" "ORDER" "PARTITION") "BY" "foo BY")
    ("INSERT" "INTO" "INSERT INTO")))

(defconst sql-smie--tokens-pairs-by-heads
  (let ((res (make-hash-table :test #'equal)))
    (pcase-dolist (`(,head ,tail ,tok) sql-smie--token-pairs)
      (let ((tok (or tok (if (listp head) tail head))))
        (dolist (head (if (listp head) head (list head)))
          (dolist (tail (if (listp tail) tail (list tail)))
            (push (cons tail tok) (gethash head res))))))
    res))

(defconst sql-smie--tokens-pairs-by-tails
  (let ((res (make-hash-table :test #'equal)))
    (pcase-dolist (`(,head ,tail ,tok) sql-smie--token-pairs)
      (let ((tok (or tok (if (listp head) tail head))))
        (dolist (head (if (listp head) head (list head)))
          (dolist (tail (if (listp tail) tail (list tail)))
            (push (cons head tok) (gethash tail res))))))
    res))

(defun sql-smie-forward-token ()
  (let* ((tok (upcase (smie-default-forward-token)))
         (paired (gethash tok sql-smie--tokens-pairs-by-heads)))
    (cond
     (paired
      (let* ((pos (point))
             (next (upcase (smie-default-forward-token)))
             (tail (assoc next paired)))
        (if tail
            (cdr tail)
          (goto-char pos)
          tok)))
     (t tok))))

(defun sql-smie-backward-token ()
  (let* ((tok (upcase (smie-default-backward-token)))
         (paired (gethash tok sql-smie--tokens-pairs-by-tails)))
    (cond
     (paired
      (let* ((pos (point))
             (next (upcase (smie-default-backward-token)))
             (head (assoc next paired)))
        (if head
            (cdr head)
          (goto-char pos)
          tok)))
     (t tok))))

(defun sql-smie-rules (kind token)
  (pcase (cons kind token)
    ;; (`(:list-intro . ,_) t) ;;FIXME!
    (`(:after . ";")
     ;; Don't align with semi-colon!
     (smie-backward-sexp token) `(column . ,(smie-indent-virtual)))
    (`(:before . "(") (smie-rule-parent))
    (`(:after . "THEN") (smie-rule-parent smie-indent-basic))
    (`(:before . "IN")
     (if (smie-rule-parent-p "FOR")
         (smie-rule-parent smie-indent-basic)))
    (`(:before . "SELECT")
     (when (smie-rule-parent-p "INSERT INTO")
       (goto-char (cadr smie--parent))
       (sql-smie-backward-token)
       `(column . ,(+ 0 (smie-indent-virtual)))))
    (`(:before . "AND")
     (if (smie-rule-parent-p "WHERE")
         (smie-rule-parent (max 0 (- (length "WHERE") (length token))))))
    (`(:before . ,(or "FROM" "WHERE" "foo BY"))
     (if (smie-rule-parent-p "SELECT")
         (smie-rule-parent (max 1 (- (length "SELECT") (length token))))))))


;;;###autoload
(defun sql-smie-enable ()
  (smie-setup sql-smie-grammar #'sql-smie-rules
              :forward-token #'sql-smie-forward-token
              :backward-token #'sql-smie-backward-token))

(provide 'sql-smie)
;;; sql-smie.el ends here
