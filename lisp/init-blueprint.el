;;; init-blueprint.el --- Support Android.bp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (package-vc-install "https://github.com/DrBluefall/blueprint-mode.git")
;; (require 'blueprint-mode)
(add-to-list 'auto-mode-alist '("\\.bp\\'" . blueprint-mode))
(autoload 'blueprint-mode "blueprint-mode" "Major mode for Android.bp" t)

(provide 'init-blueprint)
;;; init-haskell.el ends here
