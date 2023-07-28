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

;; ============
;; Verification
;; ============

(defun imp-ts-mode:logic-first (node)
  (pcase (treesit-node-type node)
    ((or "annotated_assignment" "annotated_whilestm" "annotated_seqn" "annotated_skip") (imp-ts-mode:logic-first (treesit-node-child node 0)))
    ("single_condition" node)
    ("implication" (imp-ts-mode:logic-first (treesit-node-child node 0)))))

(defun imp-ts-mode:logic-last (node)
  (pcase (treesit-node-type node)
    ("single_condition" node)
    ("implication" (imp-ts-mode:logic-last (treesit-node-child node 2)))))

(defun imp-ts-mode:precondition (node)
  (imp-ts-mode:logic-last (treesit-node-child node 0)))

(defun imp-ts-mode:check-logic (node)
  (message "Checking logic %s" node))

(defun imp-ts-mode:check-pre (node)
  (imp-ts-mode:check-logic (treesit-node-child node 0)))

(defun imp-ts-mode:check-source (node)
  (let ((post (treesit-node-child node 1)))
    (imp-ts-mode:check-logic post)
    (imp-ts-mode:verify-node (treesit-node-child node 0) (imp-ts-mode:logic-first post))))

(defun imp-ts-mode:check-assignment (node post)
  (let ((pre (imp-ts-mode:precondition node))
        (left (treesit-node-child node 1))
        (right (treesit-node-child node 3)))
    (message "checking assignment; {%s} %s := %s {%s}" pre left right post)))

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
