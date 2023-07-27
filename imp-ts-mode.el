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

(defvar imp-ts-mode-indent-rules
  (let ((offset imp-ts-mode-indent-offset))
    `((imp
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "else") parent-bol 0)
       ((node-is "end") parent-bol 0)
       ((parent-is "whilestm") parent-bol ,offset)
       ((parent-is "ifstm") parent-bol ,offset)))))

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
  (treesit-major-mode-setup))

;;;###autoload

(add-to-list 'auto-mode-alist '("\\.imp$" . imp-ts-mode))

(provide 'imp-ts-mode)

;;; imp-ts-mode.el ends here
