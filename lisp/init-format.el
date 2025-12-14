;;; init-format --- Config for formatting      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'shfmt)
(setq shfmt-arguments '("-i" "4"))

(use-package reformatter
  :init
  ;; https://github.com/purcell/emacs-reformatter/issues/35
  (reformatter-define clang-format
    :program "clang-format"
    :args (list "--style=file" (concat "--assume-filename="
                                       (file-name-nondirectory
                                        (or (buffer-file-name) input-file)))))

  ;; https://blog.csdn.net/shift_wwx/article/details/84790429
  (reformatter-define bpfmt-format
    :program "bpfmt"
    :args '("-s" "-o")
    :group 'bpfmt))

(defvar local-format-function nil
  "Function to call for formatting the buffer or region.")

(defun kk/format-buffer-or-region (&optional display-errors)
  "Format buffer or region using mode-specific formatter.
If DISPLAY-ERRORS is non-nil, display errors when formatting."
  (interactive "P")
  (if local-format-function
      (let ((beg (if (use-region-p) (region-beginning) (point-min)))
            (end (if (use-region-p) (region-end) (point-max))))
        (funcall local-format-function beg end display-errors))
    (message "No formatter function defined for the current mode.")))

(defun add-formatting-hook (mode-hook formatter)
  "Add a formatting function to the specified MODE-HOOK.
FORMATTER is the function used to format the buffer or region."
  (add-hook mode-hook
            (lambda ()
              (setq-local local-format-function formatter))))

(add-formatting-hook 'c-mode-hook 'clang-format-region)
(add-formatting-hook 'c++-mode-hook 'clang-format-region)
(add-formatting-hook 'python-mode-hook 'ruff-format-region)
(add-formatting-hook 'sh-mode-hook 'shfmt-region)
(add-formatting-hook 'blueprint-mode-hook 'bpfmt-format-region)

;; editorconfig-display-current-properties
;; (use-package editorconfig
;;   :ensure t
;;   :hook (after-init . editorconfig-mode))

(provide 'init-format)

;;; init-format.el ends here
