;; init-ai --- Config for ai      -*- lexical-binding: t; -*-

;; Commentary:

(use-package gptel
  :config
  (setq gptel-make-request #'gptel-make-request-idealab)
  (setq gptel-host "idealab.alibaba-inc.com")
  (setq gptel-endpoint "/api/openai/v1/chat/completions")
  (setq gptel-api-key (getenv "IDEALAB_API_KEY"))
  (setq gptel-stream t)
  (setq gptel-models '("qwen3-coder-plus"))
  (setq gptel-default-backend #'gptel-set-backend--qwen-idealab))

(use-package aider
  :config
  ;; For latest claude sonnet model
  ;; (setq aider-args-args ("--model" "sonnet" "--no-auto-accept-architect")) ; add --no-auto-commits if you don't want it
  (setq aider-anthropic-api-key (getenv "ANTHROPIC_API_KEY"))
  ;; or chatgpt model
  (setq aider-args '("--model" "oai-mini"))
  (setq aider-openai-api-key (getenv "OPENAI_API_KEY"))
  ;; or use your personal config file
  (setq aider-args '("--config" (expand-file-name "~/.aider.conf.yml")))

  (setq aider-magit-transients '())
  ;; Optional: Set a key binding for the transient menu
  (global-set-key (kbd "C-c M-a") 'aider-transient-menu) ; for wider screen
  ;; or use aider-transient-menu-mu2col / aider-transient-menu-mu1col, for narrow screen
  (aider-magit-setup-transients) ; add magit magit function to magit menu
  )

;; (use-package minuet
;;   :bind
;;   ("M-y" . #'minuet-complete-with-minibuffer) ; use minibuffer for completion
;;   ("M-i" . #'minuet-set-option-options) ; use overlay for completion
;;   ("C-c m" . #'minuet-configure-provider)
;;   :map minuet-active-mode-map
;;   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;   ("M-+" . #'minuet-next-suggestion) ; invoke completion or cycle to next completion
;;   ("M--" . #'minuet-previous-suggestion) ; invoke completion or cycle to previous completion
;;   ("M-=" . #'minuet-accept-suggestion) ; accept the first line of completion, or N lines with a numeric-prefix:
;;   ;; e.g. C-u 2 M-a will accept 2 lines of completion.
;;   ("M-%" . #'minuet-accept-suggestion-line)
;;   ("M-e" . #'minuet-dismiss-suggestion)

;;   :init
;;   (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

;;   :config
;;   (setq minuet-provider 'openai-compatible)
;;   ;; (minuet-set-option-options minuet-openai-compatible-options :max-tokens 8096)
;;   (plist-put minuet-openai-compatible-options :name "ideLab")
;;   (plist-put minuet-openai-compatible-options :endpoint "https://idealab.alibaba-inc.com/api/openai/v1/chat/completions")
;;   (plist-put minuet-openai-compatible-options :api-key "OPENAI_API_KEY")
;;   (plist-put minuet-openai-compatible-options :model "qwen3-coder-plus"))


(provide 'init-ai)
;; init-ai.el ends here
