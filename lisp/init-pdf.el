;;; init-pdf ---    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t)
  (pdf-tools-enable-minor-modes))

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(provide 'init-pdf)
;;; init-pdf.el ends here
