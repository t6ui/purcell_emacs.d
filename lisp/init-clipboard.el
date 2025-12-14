;; -*- coding: utf-8; lexical-binding: t; -*-

(defun copy-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n (format "%s:%s" filename (line-number-at-pos)) filename)))
      (kill-new s)
      (message "%s => clipboard&kill-ring" s))))

(defun copy-buffer-name-without-extension ()
  "Copy the buffer's file name (without extension) into the yank ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-name-base buffer-file-name)))
      (kill-new filename)
      (message "%s => clipboard&kill-ring" filename))))

(defun copy-full-path-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (message "Full file path => clipboard & yank ring")))

(defun copy-relative-path-of-current-file ()
  "Copy the relative path of the current buffer to the project root."
  (interactive)
  (when buffer-file-name
    (let* ((project-root (project-root (project-current t)))
           (relative-path (file-relative-name buffer-file-name project-root)))
      (kill-new relative-path)
      (message "Relative path => clipboard & yank ring"))))

(defun copy-directory-path-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new (file-name-directory buffer-file-name))
    (message "Directory path => clipboard & yank ring")))

(defun copy-project-path-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new (project-root (project-current)))
    (message "Project path => clipboard & yank ring")))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; maybe I should set env for SSH_TTY
;; (defun clipetty--emit (string)
;;   "Emit STRING, optionally wrapped in a DCS, to an appropriate tty."
;;   (let ((tmux    (getenv "TMUX" (selected-frame)))
;;         (term    (getenv "TERM" (selected-frame)))
;;         (ssh-tty (getenv "SSH_TTY" (selected-frame))))
;;     (if (<= (length string) clipetty--max-cut)
;;         (write-region
;;          (clipetty--dcs-wrap string tmux term ssh-tty)
;;          nil
;;          ;; (clipetty--tty ssh-tty tmux)
;;          "/dev/tty"
;;          t
;;          0)
;;       (message "Selection too long to send to terminal %d" (length string))
;;       (sit-for 1))))

(provide 'init-clipboard)
