;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar kk/highlight-pattern-file "~/.emacs.d/.hl-pattern.txt"
  "File path to store custom highlight patterns.")

(defun kk/hl-pattern-read-file ()
  "Return a list of patterns from `kk/highlight-pattern-file`."
  (when (file-exists-p kk/highlight-pattern-file)
    (with-temp-buffer
        (insert-file-contents kk/highlight-pattern-file)
      (split-string (buffer-string) "\n" t))))

(defun kk/hl-pattern (&optional arg)
  "Highlight pattern from saved list, or add one if prefix ARG is non-nil.
  Without prefix ARG (C-u), choose a pattern from file and highlight it.
  With prefix ARG, prompt for a new PATTERN and append to file if not duplicate."
  (interactive "P")
  (unless (file-exists-p kk/highlight-pattern-file)
    (write-region "" nil kk/highlight-pattern-file))
  (if arg
      (let ((pattern (read-string "Enter pattern: "))
            (patterns (kk/hl-pattern-read-file)))
        (if (member pattern patterns)
            (message "Pattern `%s` already exists." pattern)
          (append-to-file (concat pattern "\n") nil kk/highlight-pattern-file)
          (message "Added pattern: %s" pattern)))
    (let ((patterns (kk/hl-pattern-read-file)))
      (if (null patterns)
          (user-error "No patterns saved yet.")
        (let ((pattern (completing-read "Select pattern: " patterns nil t)))
          (highlight-regexp pattern))))))


(defun kk/hl-current ()
  "hight light all occurrences by selected text"
  (interactive)
  (if (region-active-p)
      (progn (deactivate-mark)
             (highlight-regexp (regexp-quote (buffer-substring (region-beginning) (region-end)))
                               (hi-lock-read-face-name)))
      (call-interactively 'highlight-symbol-at-point)))

(defun kk/hl-unhighlight-all ()
  (interactive)
  (unhighlight-regexp t))

;; Disable font-lock in non-prog-mode buffers so that hi-lock
;; will use overlay mode for highlighting instead of font-lock keywords.
;; Overlay mode applies highlights immediately and globally,
;; which is faster and more consistent for large non-code files
;; (such as logs or plain text), though it does not auto-update as you type.
(defun kk/disable-font-lock-if-not-prog ()
  "Disable `font-lock-mode` in major modes that are not derived from `prog-mode`."
  (if (derived-mode-p 'text-mode) (font-lock-mode -1)))

(add-hook 'after-change-major-mode-hook #'kk/disable-font-lock-if-not-prog)

(use-package hi-lock
  :ensure nil
  :config
  (setq hi-lock-file-patterns-policy #'(lambda (dummy) t))
  (setq hi-lock-auto-select-face t)
  (setq hi-lock-highlight-range 40000000) ; 40MB

  (defface kk/hi-yellow '((t :foreground "black" :background "yellow")) "" :group 'kk/hi-faces)
  (defface kk/hi-DeepPink '((t :foreground "black" :background "DeepPink")) "" :group 'kk/hi-faces)
  (defface kk/hi-cyan '((t :foreground "black" :background "cyan")) "" :group 'kk/hi-faces)
  (defface kk/hi-MediumPurple1 '((t :foreground "black" :background "MediumPurple1")) "" :group 'kk/hi-faces)
  (defface kk/hi-SpringGreen1 '((t :foreground "black" :background "SpringGreen1")) "" :group 'kk/hi-faces)
  (defface kk/hi-DarkOrange '((t :foreground "black" :background "DarkOrange")) "" :group 'kk/hi-faces)
  (defface kk/hi-HotPink1 '((t :foreground "black" :background "HotPink1")) "" :group 'kk/hi-faces)
  (defface kk/hi-RoyalBlue1 '((t :foreground "black" :background "RoyalBlue1")) "" :group 'kk/hi-faces)
  (defface kk/hi-OliveDrab '((t :foreground "black" :background "OliveDrab")) "" :group 'kk/hi-faces)
  (defface kk/hi-LightSkyBlue '((t :foreground "black" :background "LightSkyBlue")) "" :group 'kk/hi-faces)
  (defface kk/hi-Gold1 '((t :foreground "black" :background "Gold1")) "" :group 'kk/hi-faces)
  (defface kk/hi-LimeGreen '((t :foreground "black" :background "LimeGreen")) "" :group 'kk/hi-faces)
  (defface kk/hi-Tomato '((t :foreground "black" :background "Tomato")) "" :group 'kk/hi-faces)
  (defface kk/hi-MediumAquamarine '((t :foreground "black" :background "MediumAquamarine")) "" :group 'kk/hi-faces)

  (setq hi-lock-face-defaults
        '("kk/hi-yellow"
          "kk/hi-DeepPink"
          "kk/hi-cyan"
          "kk/hi-MediumPurple1"
          "kk/hi-SpringGreen1"
          "kk/hi-DarkOrange"
          "kk/hi-HotPink1"
          "kk/hi-RoyalBlue1"
          "kk/hi-OliveDrab"
          "kk/hi-LightSkyBlue"
          "kk/hi-Gold1"
          "kk/hi-LimeGreen"
          "kk/hi-Tomato"
          "kk/hi-MediumAquamarine")))

(provide 'init-highlight)
