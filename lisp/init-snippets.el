;;; init-snippets.el --- Snippet configuration -*- lexical-binding: t -*-

(use-package yasnippet
  ;; :bind
  ;; ("C-c C-y" . yas-insert-snippet)
  ;; ("C-c y n" . yas-new-snippet)
  :init
  (yas-global-mode 1))

(use-package auto-yasnippet
  ;; :bind
  ;; ("C-c y a" . aya-create)
  ;; ("C-c y e" . aya-expand)
  :after yasnippet)

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-snippets)

;;; init-snippets.el ends here
