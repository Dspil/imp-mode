;;; imp-ts-mode.el --- Support for imp in Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2022- ETH Zurich.

;; Keywords: lisp
;; Version: 0.0.1
;; URL: https://github.com/Dspil/imp-mode
;; Package-Requires: ((emacs "29.1"))

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; Commentary:

;; Adds syntax highlighting for Gobra and interaction capabilities
;; with the Gobra executable.

;;; Code:

;; ====================
;; Indentation and font
;; ====================

(defvar imp-ts-mode-indent-offset 2 "The indentation level for imp.")

(defvar imp-ts-mode--treesit-settings
  (treesit-font-lock-rules
   :feature 'keywords
   :language 'imp
   '(("while" @font-lock-keyword-face)
     ("end" @font-lock-keyword-face)
     ("do" @font-lock-keyword-face)
     ("if" @font-lock-keyword-face)
     ("then" @font-lock-keyword-face)
     ("else" @font-lock-keyword-face)
     ("skip" @font-lock-builtin-face))

   :feature 'logic
   :language 'imp
   '((single_condition) @font-lock-string-face)))

(defvar imp-ts-mode-indent-rules
  (let ((offset imp-ts-mode-indent-offset))
    `((imp
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "|=") parent-bol 0)
       ((node-is "else") parent-bol 0)
       ((node-is "end") parent-bol 0)
       ((node-is "then") parent-bol 0)
       ((parent-is "whilestm") parent-bol ,offset)
       ((match nil "ERROR" parent-bol ,offset))
       ((parent-is "ifstm") parent-bol ,offset)
       ((parent-is "seqn") parent-bol 0)
       ((parent-is "skip") parent-bol 0)
       ((parent-is "assignment") parent-bol 0)
       ((parent-is "annotated_whilestm") parent-bol ,offset)
       ((parent-is "annotated_ifstm") parent-bol ,offset)
       ((parent-is "annotated_seqn") parent-bol 0)
       ((parent-is "annotated_assignment") parent-bol 0)
       ((parent-is "annotated_skip") parent-bol 0)
       ((parent-is "implication") parent-bol 0)))))

;; ==============
;; Error handling
;; ==============

(defun imp-ts-mode:error (start end msg)
  (message "Error at <%s:%s>: %s" start end msg))

;; ================
;; Helper functions
;; ================

(defun imp-ts-mode:add-if-absent (item lst)
  (if (member item lst)
      lst
    (cons item lst)))

(defun imp-ts-mode:merge (lst1 lst2)
  (while lst1
    (setq lst2 (imp-ts-mode:add-if-absent (car lst1) lst2))
    (setq lst1 (cdr lst1)))
  lst2)

;; ============
;; Verification
;; ============

;; z3 query creation

(defun imp-ts-mode:query-terms-from-node (terms node)
  (pcase (treesit-node-type node)
    ("lexp" (pcase (treesit-node-type (treesit-node-child node 0))
              ("(" (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 1)))
              ("!" (let* ((data (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 1)))
                          (new-terms (car data))
                          (query (cdr data)))
                     (cons new-terms (format "(not %s)" query))))
              ((or "lexp" "laexp")
               (let* ((data-left (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 0)))
                      (new-terms-left (car data-left))
                      (query-left (cdr data-left))
                      (data-right (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 2)))
                      (new-terms-right (car data-right))
                      (query-right (cdr data-right)))
                 (cons (imp-ts-mode:merge new-terms-left new-terms-right)
                       (format "(%s %s %s)"
                               (let* ((op (treesit-node-child node 1))
                                      (op-type (treesit-node-type op)))
                                 (pcase op-type
                                   ("&&" "and")
                                   ("||" "or")
                                   ("rop" (pcase (imp-ts-mode:node-text op)
                                            ("#" "neq")
                                            (rop rop)))))
                               query-left
                               query-right))))))
    ("laexp" (pcase (treesit-node-type (treesit-node-child node 0))
               ("(" (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 1)))
               ("-" (let* ((data (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 1)))
                           (new-terms (car data))
                           (query (cdr data)))
                      (cons new-terms (format "(- 0 %s)" query))))
               ("laexp"
                (let* ((data-left (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 0)))
                       (new-terms-left (car data-left))
                       (query-left (cdr data-left))
                       (data-right (imp-ts-mode:query-terms-from-node terms (treesit-node-child node 2)))
                       (new-terms-right (car data-right))
                       (query-right (cdr data-right)))
                  (cons (imp-ts-mode:merge new-terms-left new-terms-right)
                        (format "(%s %s %s)"
                                (imp-ts-mode:node-text (treesit-node-child node 1))
                                query-left
                                query-right))))
               ("identifier" (let ((id (imp-ts-mode:node-text node))) (cons (list id) id)))
               ("numeral" (cons nil (imp-ts-mode:node-text node)))))))

(defun imp-ts-mode:data-from-single-condition (single-condition)
  (imp-ts-mode:query-terms-from-node nil (treesit-node-child single-condition 1)))

(defun imp-ts-mode:query-z3 (terms query)
  "Return t if the implication holds, nil if it doesn't."
  (with-temp-buffer
    (insert "(define-fun neq ((x!0 Int) (x!1 Int)) Bool (not (= x!0 x!1)))\n")
    (while terms
      (insert (format "(declare-const %s Int)\n" (car terms)))
      (setq terms (cdr terms)))
    (insert (format "(assert (not %s))\n(check-sat)\n" query))
    (shell-command-on-region (point-min) (point-max) "z3 -in" (current-buffer) t)
    (let ((z3-ret (string-trim (buffer-string))))
      (pcase z3-ret
        ("unsat" t)
        ("sat" nil)
        (_ (message "Unknown Z3 answer: %s" z3-ret) nil)))))

;; stringify

(defun imp-ts-mode:node-text (node)
  (let* ((str (treesit-node-text node))
         (start 0)
         (end (length str)))
    (set-text-properties start end nil str)
    str))

(defun imp-ts-mode:stringify-logic (node)
  (pcase (treesit-node-type node)
    ("lexp" (pcase (treesit-node-type (treesit-node-child node 0))
              ("(" (let* ((child (treesit-node-child node 1))
                          (str (imp-ts-mode:stringify child))
                          (grandchild (treesit-node-child child 0))
                          (grandchild-type (treesit-node-type grandchild))
                          (grandchild-op (treesit-node-child child 1))
                          (grandchild-op-type (treesit-node-type grandchild-op)))
                     (if (and (equal grandchild-type "lexp") (equal grandchild-op-type "||"))
                         (format "(%s)" str)
                       str)))
              ("!" (let* ((str (imp-ts-mode:stringify (treesit-node-child node 1))))
                     (format "!%s" str)))
              ((or "lexp" "laexp")
               (let* ((str-left (imp-ts-mode:stringify (treesit-node-child node 0)))
                      (str-right (imp-ts-mode:stringify (treesit-node-child node 2))))
                 (let* ((op (treesit-node-child node 1))
                        (op-type (treesit-node-type op))
                        (paren (if (equal op-type "||") t nil)))
                   (format "%s%s%s"
                           str-left
                           (pcase op-type
                             ("rop" (imp-ts-mode:node-text op))
                             (optext optext))
                           str-right))))))
    ("laexp" (pcase (treesit-node-type (treesit-node-child node 0))
               ("(" (imp-ts-mode:stringify (treesit-node-child node 1)))
               ("-" (let ((str (imp-ts-mode:stringify (treesit-node-child node 1))))
                      (format "(-%s)" str)))
               ("laexp"
                (let* ((str-left (imp-ts-mode:stringify (treesit-node-child node 0)))
                       (str-right (imp-ts-mode:stringify (treesit-node-child node 2))))
                  (format "(%s%s%s)"
                          str-left
                          (imp-ts-mode:node-text (treesit-node-child node 1))
                          str-right)))
               ("identifier" (let ((id (imp-ts-mode:node-text node))) id))
               ("numeral" (imp-ts-mode:node-text node))))))

(defun imp-ts-mode:stringify-to-logic (node)
  (pcase (treesit-node-type node)
    ((or "lexp" "bexp") (pcase (treesit-node-type (treesit-node-child node 0))
                          ("(" (let* ((child (treesit-node-child node 1))
                                      (str (imp-ts-mode:stringify child))
                                      (grandchild (treesit-node-child child 0))
                                      (grandchild-type (treesit-node-type grandchild))
                                      (grandchild-op (treesit-node-child child 1))
                                      (grandchild-op-type (treesit-node-type grandchild-op)))
                                 (if (and (equal grandchild-type "lexp") (equal grandchild-op-type "||"))
                                     (format "(%s)" str)
                                   str)))
                          ((or "!" "not") (let* ((str (imp-ts-mode:stringify (treesit-node-child node 1))))
                                            (format "!%s" str)))
                          ((or "lexp" "laexp")
                           (let* ((str-left (imp-ts-mode:stringify (treesit-node-child node 0)))
                                  (str-right (imp-ts-mode:stringify (treesit-node-child node 2))))
                             (let* ((op (treesit-node-child node 1))
                                    (op-type (treesit-node-type op)))
                               (format "%s%s%s"
                                       str-left
                                       (pcase op-type
                                         ("rop" (imp-ts-mode:node-text op))
                                         ("and" "&&")
                                         ("or" "||")
                                         (optext optext))
                                       str-right))))))
    ((or "laexp" "aexp") (pcase (treesit-node-type (treesit-node-child node 0))
                           ("(" (imp-ts-mode:stringify (treesit-node-child node 1)))
                           ("-" (let ((str (imp-ts-mode:stringify (treesit-node-child node 1))))
                                  (format "(-%s)" str)))
                           ("laexp"
                            (let* ((str-left (imp-ts-mode:stringify (treesit-node-child node 0)))
                                   (str-right (imp-ts-mode:stringify (treesit-node-child node 2))))
                              (format "(%s%s%s)"
                                      str-left
                                      (imp-ts-mode:node-text (treesit-node-child node 1))
                                      str-right)))
                           ("identifier" (let ((id (imp-ts-mode:node-text node))) id))
                           ("numeral" (imp-ts-mode:node-text node))))))

;; tree traversal

(defun imp-ts-mode:translate-typ (typ)
  (if (equal typ "aexp")
      "laexp"
    typ))

(defun imp-ts-mode:node-child-no-paren (node)
  (if (equal (treesit-node-type node) "single_condition")
      (imp-ts-mode:node-child-no-paren (treesit-node-child node 1))
    (if (equal (treesit-node-type (treesit-node-child node 0)) "(")
        (imp-ts-mode:node-child-no-paren (treesit-node-child node 1))
      node)))

(defun imp-ts-mode:logic-first (node)
  (pcase (treesit-node-type node)
    ((or "annotated_assignment" "annotated_whilestm" "annotated_seqn" "annotated_skip")
     (imp-ts-mode:logic-first (treesit-node-child node 0)))
    ("single_condition" node)
    ("implication" (imp-ts-mode:logic-first (treesit-node-child node 0)))))

(defun imp-ts-mode:logic-last (node)
  (pcase (treesit-node-type node)
    ("single_condition" node)
    ("implication" (imp-ts-mode:logic-last (treesit-node-child node 2)))))

(defun imp-ts-mode:precondition (node)
  (imp-ts-mode:logic-last (treesit-node-child node 0)))

(defun imp-ts-mode:check-logic (node)
  (message "Checking logic %s" node)
  (pcase (treesit-node-type node)
    ("implication" (let* ((left (treesit-node-child node 0))
                          (right (treesit-node-child node 2))
                          (last-left (imp-ts-mode:logic-last left))
                          (first-right (imp-ts-mode:logic-first right))
                          (data-left (imp-ts-mode:data-from-single-condition last-left))
                          (terms-left (car data-left))
                          (query-left (cdr data-left))
                          (data-right (imp-ts-mode:data-from-single-condition first-right))
                          (terms-right (car data-right))
                          (query-right (cdr data-right))
                          (terms (imp-ts-mode:merge terms-left terms-right))
                          (query (format "(=> %s %s)" query-left query-right)))
                     (imp-ts-mode:check-logic left)
                     (imp-ts-mode:check-logic right)
                     (when (not (imp-ts-mode:query-z3 terms query))
                       (imp-ts-mode:error (treesit-node-start last-left) (treesit-node-end first-right) "Implication might not hold."))))
    (_ nil)))

(defun imp-ts-mode:check-pre (node)
  (imp-ts-mode:check-logic (treesit-node-child node 0)))

(defun imp-ts-mode:check-source (node)
  (let ((post (treesit-node-child node 1)))
    (imp-ts-mode:check-logic post)
    (imp-ts-mode:verify-node (treesit-node-child node 0) (imp-ts-mode:logic-first post))))

(defun imp-ts-mode:check-assignment-rule (pre post left right &optional replaced)
  (let ((pre-typ (treesit-node-type pre))
        (post-typ (imp-ts-mode:translate-typ (treesit-node-type post))))
    (when  (or (equal `(,pre-typ . ,post-typ) '("lexp" . "lexp"))
               (equal `(,pre-typ . ,post-typ) '("laexp" . "laexp")))
      (let* ((fst-pre (treesit-node-child pre 0))
             (fst-post (treesit-node-child post 0))
             (fst-pre-typ (treesit-node-type fst-pre))
             (fst-post-typ (imp-ts-mode:translate-typ (treesit-node-type fst-post))))
        (pcase `(,fst-pre-typ . ,fst-post-typ)
          ((or '("lexp" . "lexp") '("laexp" . "laexp"))
           (when (equal (imp-ts-mode:node-text (treesit-node-child pre 1)) (imp-ts-mode:node-text (treesit-node-child post 1)))
             (and (imp-ts-mode:check-assignment-rule (imp-ts-mode:node-child-no-paren fst-pre)
                                                     (imp-ts-mode:node-child-no-paren fst-post)
                                                     left
                                                     right
                                                     replaced)
                  (imp-ts-mode:check-assignment-rule (imp-ts-mode:node-child-no-paren (treesit-node-child pre 2))
                                                     (imp-ts-mode:node-child-no-paren (treesit-node-child post 2))
                                                     left
                                                     right
                                                     replaced))))
          ((or '("!" . "!") '("-" . "-"))
           (imp-ts-mode:check-assignment-rule (imp-ts-mode:node-child-no-paren (treesit-node-child pre 1))
                                              (imp-ts-mode:node-child-no-paren (treesit-node-child post 1))
                                              left
                                              right
                                              replace))
          ((guard (and (equal fst-post-typ "identifier")
                       (equal left (imp-ts-mode:node-text fst-post))
                       (not replaced)))
           (imp-ts-mode:check-assignment-rule pre right left right t))
          ((or '("identifier" . "identifier") '("numeral" . "numeral"))
           (equal (imp-ts-mode:node-text fst-pre) (imp-ts-mode:node-text fst-post))))))))

(defun imp-ts-mode:check-assignment (node post)
  (let ((pre (imp-ts-mode:precondition node))
        (left (treesit-node-child node 1))
        (right (treesit-node-child node 3)))
    (if (not (imp-ts-mode:check-assignment-rule (imp-ts-mode:node-child-no-paren pre)
                                                (imp-ts-mode:node-child-no-paren post)
                                                (imp-ts-mode:node-text left)
                                                right))
        (imp-ts-mode:error (treesit-node-start left)
                           (treesit-node-end right)
                           "Assignment rule not applied properly."))))

(defun imp-ts-mode:check-whilestm (node post)
  (let* ((pre (imp-ts-mode:precondition node))
         (condition (treesit-node-child node 2))
         (body (treesit-node-child node 4))
         (inv-pre (imp-ts-mode:logic-first (treesit-node-child body 0)))
         (body-post (treesit-node-child node 5))
         (inv-post (imp-ts-mode:logic-last body-post)))
    (imp-ts-mode:check-logic body-post)
    (imp-ts-mode:verify-node body (imp-ts-mode:logic-first body-post))
    (message "Checking while: {%s} while %s do {%s} %s {%s} end {%s}" pre condition inv-pre body inv-post post)))

(defun imp-ts-mode:check-ifstm (node post)
  (let* ((pre (imp-ts-mode:precondition node))
         (condition (treesit-node-child node 2))
         (then (treesit-node-child node 4))
         (then-pre (imp-ts-mode:logic-first (treesit-node-child then 0)))
         (then-body-post (treesit-node-child node 5))
         (then-post (imp-ts-mode:logic-last then-body-post))
         (else (treesit-node-child node 7))
         (else-pre (imp-ts-mode:logic-first (treesit-node-child else 0)))
         (else-body-post (treesit-node-child node 8))
         (else-post (imp-ts-mode:logic-last else-body-post)))
    (imp-ts-mode:check-logic then-body-post)
    (imp-ts-mode:check-logic else-body-post)
    (imp-ts-mode:verify-node then (imp-ts-mode:logic-first then-body-post))
    (imp-ts-mode:verify-node else (imp-ts-mode:logic-first else-body-post))
    (message "Checking if: {%s} if %s then {%s} %s {%s} else {%s} %s {%s} end {%s}" pre condition then-pre then then-post else-pre else else-post post)))

(defun imp-ts-mode:check-skip (node post)
  (let ((pre (imp-ts-mode:precondition node)))
    (message "checking skip; {%s} skip {%s}" pre post)))

(defun imp-ts-mode:check-seqn (node post)
  (let ((stm (treesit-node-child node 0)))
    (while stm
      (let* ((next-stm (treesit-node-next-sibling (treesit-node-next-sibling stm)))
             (cur-post (if next-stm
                           (imp-ts-mode:logic-first (treesit-node-child next-stm 0))
                         post)))
        (imp-ts-mode:verify-node stm cur-post)
        (setq stm next-stm)))))

(defun imp-ts-mode:verify-node (node &optional post)
  (pcase (treesit-node-type node)
    ("source_file" (imp-ts-mode:check-source node))
    ((or "whilestm" "ifstm" "seqn" "skip" "assignment") (message "Program does not contain hoare logic proof."))
    ("annotated_assignment" (imp-ts-mode:check-pre node) (imp-ts-mode:check-assignment node post))
    ("annotated_whilestm" (imp-ts-mode:check-pre node) (imp-ts-mode:check-whilestm node post))
    ("annotated_ifstm" (imp-ts-mode:check-pre node) (imp-ts-mode:check-ifstm node post))
    ("annotated_seqn" (imp-ts-mode:check-seqn node post))
    ("annotated_skip" (imp-ts-mode:check-pre node) (imp-ts-mode:check-skip node post))))

;; =====================
;; Interactive functions
;; =====================

(defun imp-ts-mode:verify ()
  "Verify the hoare logic in the file"
  (interactive)
  (let ((tree (treesit-buffer-root-node)))
    (imp-ts-mode:verify-node tree)))

;; ==========
;; Major mode
;; ==========

(define-derived-mode imp-ts-mode fundamental-mode
  "imp mode with tree-sitter"
  "Major mode for editing imp"
  (electric-indent-local-mode nil)
  (treesit-parser-create 'imp)
  (setq-local treesit-simple-indent-rules imp-ts-mode-indent-rules)
  (setq-local treesit-font-lock-settings imp-ts-mode--treesit-settings)
  (setq-local treesit-font-lock-feature-list
              '((keywords logic)))
  (treesit-major-mode-setup))

;;;###autoload

(add-to-list 'auto-mode-alist '("\\.imp$" . imp-ts-mode))

(provide 'imp-ts-mode)

;;; imp-ts-mode.el ends here
