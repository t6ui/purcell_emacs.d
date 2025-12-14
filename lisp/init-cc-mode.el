;; -*- coding: utf-8; lexical-binding: t; -*-

(defun jump-to-word-before-parenthesis ()
  "Jump to the beginning of the word immediately before the first parenthesis on the current line."
  (let ((current-point (point)))
    (beginning-of-line)
    (if (search-forward "(" (line-end-position) t)
        (progn
          (backward-char 2)  ; Move back to the character before the parenthesis
          (backward-word 1)) ; Move back to the beginning of the word
      (goto-char current-point) ; If no parenthesis found, return to original position
      (message "No parenthesis found on the current line."))))

(defun my-cpp-forward-function ()
  (interactive)
  (evil-forward-section-begin)
  (jump-to-word-before-parenthesis))

(defun my-cpp-backward-function ()
  (interactive)
  (beginning-of-line)
  (evil-backward-section-begin)
  (jump-to-word-before-parenthesis))

(use-package cc-mode
  :mode ("\\.inl\\'" . c++-mode)
  :hook ((c-mode . my-c-mode-setup)
         (c++-mode . my-c-mode-setup)
         (c-mode-common . (lambda () (setq indent-tabs-mode nil))))
  :init
  (defun my-c-mode-setup ()
    "C/C++ setup."
    (push '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1) imenu-generic-expression )
    ;; Use `ff-find-other-file' to open C/C++ header
    (setq cc-search-directories '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include" "../*"))
    ;; make a #define be left-aligned
    (setq c-electric-pound-behavior (quote (alignleft))))
  :config
  ;; (setq flymake-diagnostic-functions '(flymake-flycheck:c/c++-clang))

  ;; use more popular style "linux" instead of "gnu",
  (c-add-style
   "my-linux-style"
   '("linux"
     (c-basic-offset . 4)
     (tab-width . 4)))

  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "my-linux-style"))))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
