;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'orderless)

  (require 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))))

  ;; ;; https://github.com/oantolin/orderless/issues/140
  ;; (defun case-dispatcher (target _index _total)
  ;;   (when (equal "&&" target)
  ;;     (setq-local orderless-smart-case nil completion-ignore-case nil)
  ;;     '(orderless-literal . "")))
  ;; (add-to-list 'orderless-style-dispatchers #'case-dispatcher)
  )

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (setq vertico-cycle t)
  (setq vertico-count 10)
  (with-eval-after-load 'vertico
    (vertico-indexed-mode)

    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
    (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)

    ;; https://github.com/oantolin/orderless/issues/154
    (defun kk/orderless-toggle-smart-case ()
      (interactive)
      (message
       "Smart case: %s"
       (setq-local orderless-smart-case (not orderless-smart-case)))
      (when (boundp 'vertico--input)
        (setq-local vertico--input t))) ;; Enforce Vertico refresh
    (keymap-set vertico-map "C-c ~" #'kk/orderless-toggle-smart-case)

    ;; https://github.com/oantolin/orderless/issues/136
    ;; https://github.com/oantolin/orderless/issues/86
    (defun kk/toggle-completion-styles ()
      "Toggle completion styles between `orderless basic` and `substring`."
      (interactive)
      (if (equal completion-styles '(substring))
          (setq-local completion-styles '(orderless basic))
        (setq-local completion-styles '(substring)))
      (when (boundp 'vertico--input)
        (setq-local vertico--input t))
      (message "Completion styles: %S" completion-styles))

    (define-key vertico-map (kbd "C-c C-t") #'kk/toggle-completion-styles)

    ;; see https://github.com/minad/vertico/wiki
    (defun kk/vertico-restrict-to-matches ()
      (interactive)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert " ")
        (add-text-properties (minibuffer-prompt-end) (point-max)
                             '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
    (define-key vertico-map (kbd "C-c C-l") #'kk/vertico-restrict-to-matches)
    ;; (define-key vertico-map (kbd "S-SPC") #'+vertico-restrict-to-matches)

    (keymap-global-set "M-R" #'vertico-repeat)
    (keymap-set vertico-map "M-P" #'vertico-repeat-previous)
    (keymap-set vertico-map "M-N" #'vertico-repeat-next)
    (keymap-set vertico-map "S-<prior>" #'vertico-repeat-previous)
    (keymap-set vertico-map "S-<next>" #'vertico-repeat-next))

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (when (maybe-require-package 'embark)
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-c C-c") 'embark-act)))
  ;; https://github.com/purcell/whole-line-or-region/issues/30#issuecomment-3388095018
  (with-eval-after-load 'embark
    (push 'embark--mark-target
          (alist-get 'whole-line-or-region-delete-region
                     embark-around-action-hooks)))

  (when (maybe-require-package 'consult)
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-P")))

    (sanityinc/no-consult-preview
     consult-ripgrep consult-man
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list current-prefix-arg
                         (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (if-let* ((s (symbol-at-point)))
                               (symbol-name s)))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (when (executable-find "rg")
      (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point))

    (when (and (executable-find "rg"))
      (defun kk/consult-rg (&optional dir initial)
        (interactive (list current-prefix-arg
                           (when (region-active-p)
                             (prog1 (concat (buffer-substring-no-properties
                                             (region-beginning) (region-end)) "#")
                               (deactivate-mark)))))
        (consult-ripgrep dir initial))

      (sanityinc/no-consult-preview kk/consult-rg))

    (defun kk/swiper (&optional initial start)
      (interactive
       (list (when (region-active-p)
               (prog1 (buffer-substring-no-properties (region-beginning) (region-end))
                 (deactivate-mark)))
             (not (not current-prefix-arg))))
      (consult-line initial start))

    (defun kk/consult-rg-current ()
      "Call `consult-ripgrep' for the current buffer (a single file)."
      (interactive)
      (let ((consult-project-function (lambda (x) nil)))
        (consult-ripgrep (list (shell-quote-argument buffer-file-name)))))

    (defun kk/consult-rg-directory ()
      "Call `consult-ripgrep' for the current buffer's directory."
      (interactive)
      (let* ((file-directory (file-name-directory buffer-file-name))
             (consult-project-function (lambda (_) file-directory)))
        (consult-ripgrep file-directory)))

    (defun kk/consult-grep-current ()
      "Call `consult-grep' for the current buffer (a single file)."
      (interactive)
      (consult-grep (list (shell-quote-argument buffer-file-name))))

    (defun kk/consult-fd-similar (&optional dir initial)
      (interactive (list current-prefix-arg
                         (when buffer-file-name
                           (file-name-base buffer-file-name))))
      (consult-fd dir initial))

    (defun kk/consult-fd (&optional dir initial)
      (interactive (list current-prefix-arg
                         (when (region-active-p)
                           (concat (buffer-substring-no-properties
                                    (region-beginning) (region-end)) "#"))))
      (consult-fd dir initial))

    (with-eval-after-load 'consult
      (require 'consult)
      (require 'popper)

      (defun kk/buffer-candidate-with-info (buf)
        "Return a `(display . buffer)` pair for BUF, with mode or popper status info."
        (with-current-buffer buf
          (let* ((name (buffer-name))
                 (mode (format "%s" major-mode))
                 (status (when (boundp 'popper-popup-status)
                           popper-popup-status))
                 (info (cond
                        (status (format "[%s]" status))
                        (t (format "[%s]" mode)))))
            ;; consult--buffer-pair create `(string . buffer)` struct
            (cons (format "%s %s" name info) buf))))

      (defvar kk/popper-buffer-source
        (list :name     "Popper Buffers"
              :narrow   ?x
              :category 'buffer
              :face     'consult-buffer
              :history  'buffer-name-history
              :state    #'consult--buffer-state
              :items
              (lambda ()
                (mapcar #'kk/buffer-candidate-with-info
                        (seq-filter
                         (lambda (buf)
                           (with-current-buffer buf
                             (and (boundp 'popper-popup-status)
                                  popper-popup-status)))
                         (buffer-list))))))

      (add-to-list 'consult-buffer-sources 'kk/popper-buffer-source 'append)

      (defun kk/consult-popper-buffers ()
        "Consult Popper buffers."
        (interactive)
        (consult--multi (list kk/popper-buffer-source)
                        :prompt "Popper Buffer: "
                        :history 'buffer-name-history))

      (defvar org-source
        (list :name     "Org Buffer"
              :category 'buffer
              :narrow   ?o
              :face     'consult-buffer
              :history  'buffer-name-history
              :state    #'consult--buffer-state
              :new
              (lambda (name)
                (with-current-buffer (get-buffer-create name)
                  (insert "#+title: " name "\n\n")
                  (org-mode)
                  (consult--buffer-action (current-buffer))))
              :items
              (lambda ()
                (consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair))))

      (add-to-list 'consult-buffer-sources 'org-source 'append))

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key (kbd "M-=") #'kk/consult-popper-buffers)  ;; quoted-insert

    (when (maybe-require-package 'embark-consult)
      (require 'embark-consult))))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
