;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'git-modes)
(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))

(require-package 'git-link)

(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk 'all)

  (sanityinc/fullframe-mode 'magit-status-mode)

  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)

  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)))


(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up)

  (defun kk/magit-gerrit-push (&optional branch)
    (interactive
     (let ((up (magit-get-upstream-branch)))
       (list (if up
                 (string-remove-prefix (concat (car (split-string up "/")) "/") up)
               (read-string "Gerrit branch: ")))))
    (let ((remote (magit-get-upstream-remote)))
      (magit-git-command-topdir (format "git push %s HEAD:refs/for/%s" remote branch))))

  (transient-append-suffix 'magit-push "u"
    '("P" "push <remote> HEAD:refs/for/<upstream>" kk/magit-gerrit-push))

  (setq magit-log-margin-show-committer-date t))

(maybe-require-package 'magit-todos)

(add-hook 'git-commit-mode-hook 'goto-address-mode)


;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))



;;; git-svn support

;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))

(with-eval-after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

(defvar kk/ediff-buffer-A nil)
(defvar kk/ediff-buffer-B nil)

(defun kk/ediff-new ()
  "Open two empty buffers and prepare for Ediff."
  (interactive)
  (setq kk/ediff-buffer-A
        (or (get-buffer "*Diff-A*")
            (generate-new-buffer "*Diff-A*")))
  (setq kk/ediff-buffer-B
        (or (get-buffer "*Diff-B*")
            (generate-new-buffer "*Diff-B*")))
  (delete-other-windows)
  (split-window-right)
  (switch-to-buffer kk/ediff-buffer-A)
  (other-window 1)
  (switch-to-buffer kk/ediff-buffer-B)
  (message "Fill in both buffers, then use kk/ediff-go to compare."))

(autoload 'ediff-other-buffer "ediff"
  "Return the other buffer for comparison in Ediff." nil nil)

(defun kk/ediff-buffers-wordwise (buffer-A buffer-B &optional startup-hooks job-name)
  "Compare BUFFER-A and BUFFER-B in wordwise mode using Ediff."
  (interactive
   (let (bf)
     (list (setq bf (read-buffer "Buffer A to compare: "
                                 (ediff-other-buffer "") t))
           (read-buffer "Buffer B to compare: "
                        (progn
                          ;; realign buffers so that two visible bufs will be
                          ;; at the top
                          (save-window-excursion (other-window 1))
                          (ediff-other-buffer bf))
                        t))))

  (setq buffer-A (get-buffer buffer-A)
        buffer-B (get-buffer buffer-B)
        job-name (or job-name 'ediff-buffers-wordwise))
  (cl-assert buffer-A nil "Not a live buffer: %s" buffer-A)
  (cl-assert buffer-B nil "Not a live buffer: %s" buffer-B)
  (ediff-regions-internal buffer-A
                          (with-current-buffer buffer-A
                            (point-min))
                          (with-current-buffer buffer-A
                            (point-max))
                          buffer-B
                          (with-current-buffer buffer-B
                            (point-min))
                          (with-current-buffer buffer-B
                            (point-max))
                          startup-hooks
                          job-name
                          'word-mode
                          nil))

(defun kk/ediff-go ()
  "Run Ediff on the two prepared buffers."
  (interactive)
  (unless (and (buffer-live-p kk/ediff-buffer-A)
               (buffer-live-p kk/ediff-buffer-B))
    (error "Buffers not prepared, run kk/ediff-start first"))
  (ediff-buffers (buffer-name kk/ediff-buffer-A)
                 (buffer-name kk/ediff-buffer-B)))

(provide 'init-git)
;;; init-git.el ends here
