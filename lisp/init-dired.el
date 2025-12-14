;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun open-dired-in-two-windows ()
  "Open two Dired windows side by side."
  (interactive)
  (let ((dir1 (read-directory-name "First directory: "))
        (dir2 (read-directory-name "Second directory: ")))
    (dired dir1)
    (dired-other-window dir2)))

;; todo: porting doom-emacs function +dired/quit-all

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(if *is-a-mac*
    (setq dired-listing-switches "-AlBhGt")
  (setq dired-listing-switches
        "-AlBhG --time-style=long-iso --group-directories-first"))

(when (maybe-require-package 'diredfl)
  (with-eval-after-load 'dired
    (diredfl-global-mode)
    (require 'dired-x)
    (require 'dired-aux)))

(add-hook 'dired-mode-hook 'diredfl-mode)
(add-hook 'dirvish-directory-view-mode-hook 'diredfl-mode)

(with-eval-after-load 'dired-x
  ;; (setq dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq-default dired-dwim-target t)
  (setq mouse-1-click-follows-link nil)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-kill-when-opening-new-dired-buffer t) ; added in Emacs 28
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)

  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)

  (bind-keys :map dired-mode-map
             ("," . dired-create-directory)
             ("." . dired-create-empty-file)
             ("i" . wdired-change-to-wdired-mode)
             ("I" . dired-insert-subdir)
             ("K" . dired-kill-subdir)
             ("[" . dired-prev-dirline)
             ("]" . dired-next-dirline)
             ("<" . beginning-of-buffer)
             (">" . end-of-buffer)
             ("b" . dired-up-directory)
             ("^" . mode-line-other-buffer)))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(with-eval-after-load 'image-dired
  (setq image-dired-thumb-size 256)
  (setq image-dired-marking-shows-next nil)
  (bind-keys :map image-dired-thumbnail-mode-map
             ("n" . image-dired-next-line)
             ("p" . image-dired-previous-line)
             ("i" . image-dired-forward-image)
             ("o" . image-dired-backward-image)))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  (setq dirvish-path-separators (list "~" "/" "/"))
  (setq dirvish-quick-access-entries
        '(("h" "~/"                          "Home")
          ("d" "~/.dotfiles/"                "Dotfiles")
          ("e" "~/.emacs.d/"                 "Emacs user directory")
          ("c" "/workspace/code/"            "Code")))
  ;; (dirvish-side-follow-mode)
  ;; (dirvish-peek-mode)
  ;; (add-hook 'dirvish-setup-hook 'dirvish-emerge-mode)
  ;; (setq dirvish-attributes
  ;;       '(vc-state file-size git-msg subtree-state nerd-icons collapse file-time))
  ;; (setq dirvish-subtree-state-style 'nerd)
  ;; (setq dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index)))
  ;; (setq dirvish-header-line-height '(25 . 35))
  ;; (setq dirvish-side-width 38)
  ;; (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  :config
  (bind-keys :map dirvish-mode-map
             ("TAB" . dirvish-subtree-toggle)
             ("M-n" . dirvish-history-go-forward)
             ("M-p" . dirvish-history-go-backward)
             ("h"   . dirvish-history-jump)
             ;; ("a"   . dirvish-quick-access)
             ;; ("f"   . dirvish-file-info-menu)
             ;; ("v"   . dirvish-vc-menu)
             ;; ("*"   . dirvish-mark-menu)
             ;; ("N"   . dirvish-narrow)
             ;; ("M-e" . dirvish-emerge-menu)
             ;; ("M-t" . dirvish-layout-toggle)
             ;; ("M-s" . dirvish-setup-menu)
             ;; ("M-j" . dirvish-fd-jump)
             ;; ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
             ;; ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
             ;; ([remap dired-do-copy] . dirvish-yank-menu)
             ("/" . dirvish-narrow)))

(provide 'init-dired)
;;; init-dired.el ends here
