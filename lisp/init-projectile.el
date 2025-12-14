;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)

  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden -0"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ;; (add-to-list 'projectile-project-root-files-bottom-up ".project")

    (with-eval-after-load 'citre
      (setq citre-project-root-function #'projectile-project-root))
    (with-eval-after-load 'consult
      (setq consult-project-function (lambda (_) (projectile-project-root)))))

  (maybe-require-package 'ibuffer-projectile))

(use-package project
  :config
  ;; check project-find-functions & (project-root (project-current))
  (add-to-list 'project-vc-extra-root-markers ".projectile"))

(provide 'init-projectile)
;;; init-projectile.el ends here
