;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package citre
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)

  (defun my-remove-t-from-completion-at-point-functions ()
    "Remove 't from `completion-at-point-functions`."
    (setq-local completion-at-point-functions
                (remq 't completion-at-point-functions)))

  (add-hook 'citre-mode-hook #'my-remove-t-from-completion-at-point-functions)

  :config
  (setq imenu-sort-function 'imenu--sort-by-position)
  (setq
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   ;; citre-project-root-function #'projectile-project-root

   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache

   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)

   ;; citre-tags-imenu-create-tags-file-threshold nil
   ;; citre-tags-imenu-create-tags-file-threshold 0
   citre-tags-in-buffer-backends '(global tags))
  (setq citre-ctags-default-options
        "-o
%TAGSFILE%
-R
--options=/home/tangzihui.tzh/.ctags
"
        ))

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(defun kk/xref-find-definitions-prompt ()
  "Find definitions with prompt for identifier."
  (interactive)
  (let ((identifier (read-string "Enter identifier: ")))
    (xref--find-definitions identifier nil)))

(defun kk/citre-jump-or-grep (&optional reference)
  "Perform citre jump or consult ripgrep based on context."
  (interactive "P")
  (if reference
      (let ((current-prefix-arg '(4)))
        (call-interactively 'xref-find-definitions))
    (pcase-let ((`(,backend . ,tags)
                 (citre-get-backend-and-definitions))
                (identifier (if (region-active-p)
                                (buffer-substring-no-properties (region-beginning) (region-end))
                              (thing-at-point 'symbol t))))
      (if (null tags)
          (consult-ripgrep current-prefix-arg identifier)
        (progn (citre-jump-show tags)
               (citre-backend-after-jump backend))))))

;; stop asking “Keep current list of tags tables also”
;; (setq tags-table-list nil)
(setq tags-add-tables nil)

(provide 'init-ctags)
