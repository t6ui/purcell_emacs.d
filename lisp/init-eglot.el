;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)
  (setq eglot-ignored-server-capabilities
        '(:hoverProvider
          :documentHighlightProvider
          :documentFormattingProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :renameProvider
          :documentLinkProvider)))


(provide 'init-eglot)
;;; init-eglot.el ends here
