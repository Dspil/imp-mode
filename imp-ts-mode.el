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

(setq imp-ts-mode--treesit-settings
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
       ((node-is "else") parent-bol 0)
       ((node-is "end") parent-bol 0)
       ((node-is "if") parent-bol 0)
       ((node-is "|=") parent-bol 0)
       ((node-is "while") parent-bol 0)
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

(defun imp-ts-mode:verify ()
  "Verify the hoare logic in the file"
  (interactive)
  (let ((tree (treesit-buffer-root-node)))
    (when (treesit))))

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
