;;; init-terminals.el --- Terminal emulators          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eat)
  (defun sanityinc/on-eat-exit (process)
    (when (zerop (process-exit-status process))
      (kill-buffer)
      (unless (eq (selected-window) (next-window))
        (delete-window))))
  (add-hook 'eat-exit-hook 'sanityinc/on-eat-exit)

  (with-eval-after-load 'eat
    ;; (custom-set-variables
    ;;  `(eat-semi-char-non-bound-keys
    ;;    (quote ,(cons  [?\e ?w] (cl-remove [?\e ?w] eat-semi-char-non-bound-keys :test 'equal)))))
    ;; (add-to-list 'eat-semi-char-non-bound-keys [?\e ?w])
    ;; (add-to-list 'eat-semi-char-non-bound-keys [?\e ?=])
    (custom-set-variables
     `(eat-semi-char-non-bound-keys
       (quote ,(append '([?\e ?w] [?\e ?o])
                       (cl-remove-if
                        (lambda (key)
                          (member key '([?\e ?w] [?\e ?o])))
                        eat-semi-char-non-bound-keys)))))
    (define-key eat-semi-char-mode-map (kbd "C-q") nil))

  (defcustom sanityinc/eat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "t") 'eat-other-window)
      map)
    "Prefix map for commands that create and manipulate eat buffers.")
  (fset 'sanityinc/eat-map sanityinc/eat-map)

  (setq-default eat-term-scrollback-size (* 2 1024 1024))

  (defun sanityinc/eat-term-get-suitable-term-name (&optional display)
    "Version of `eat-term-get-suitable-term-name' which uses better-known TERM values."
    (let ((colors (display-color-cells display)))
      (cond ((> colors 8) "xterm-256color")
            ((> colors 1) "xterm-color")
            (t "xterm"))))
  (setq eat-term-name 'sanityinc/eat-term-get-suitable-term-name)

  (global-set-key (kbd "C-c t") 'sanityinc/eat-map))

;; (general-define-key "C-q" 'bury-buffer)
(global-set-key (kbd "C-q") #'bury-buffer)  ;; quoted-insert


(provide 'init-terminals)
;;; init-terminals.el ends here
