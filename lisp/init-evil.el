;;; init-evil --- Config for evil       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init
  (setq evil-move-cursor-back t
        evil-default-cursor t
        evil-symbol-word-search t
        evil-kill-on-visual-paste nil)
  (evil-mode 1)
  :config
  (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
        evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
        evil-insert-state-tag   (propertize "[I]" 'face '((:background "red") :foreground "white"))
        evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
        evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
        evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

  (defvar my-initial-evil-state-alist
    '((minibuffer-inactive-mode . emacs)
      (calendar-mode . emacs)
      (special-mode . emacs)
      (grep-mode . emacs)
      (Info-mode . emacs)
      (term-mode . emacs)
      (sdcv-mode . emacs)
      (anaconda-nav-mode . emacs)
      (log-edit-mode . emacs)
      (vc-log-edit-mode . emacs)
      (magit-log-edit-mode . emacs)
      (erc-mode . emacs)
      (diff-mode . emacs)
      (neotree-mode . emacs)
      (gud-mode . emacs)
      (help-mode . emacs)
      (eshell-mode . emacs)
      (shell-mode . emacs)
      (vterm-mode . emacs)
      (xref--xref-buffer-mode . emacs)
      (epa-key-list-mode . emacs)
      (weibo-timeline-mode . emacs)
      (weibo-post-mode . emacs)
      (Man-mode . emacs)
      (woman-mode . emacs)
      (sr-mode . emacs)
      (profiler-report-mode . emacs)
      (dired-mode . emacs)
      (compilation-mode . emacs)
      (speedbar-mode . emacs)
      (occur-mode . emacs)
      (eat-mode . emacs)
      (messages-buffer-mode . emacs))
    "Default evil state per major mode.")

  (defun my-apply-evil-initial-states ()
    "Apply custom initial Evil states for specific major modes."
    (dolist (pair my-initial-evil-state-alist)
      (ignore-errors
        (evil-set-initial-state (car pair) (cdr pair)))))

  (my-apply-evil-initial-states)
  ;; minior-mode
  (add-hook 'magit-blob-mode-hook #'evil-emacs-state)

  (with-eval-after-load 'citre
    (evil-add-command-properties #'citre-jump :jump t)
    (evil-add-command-properties #'citre-jump-or-grep :jump t))

  (defun kk/evil-query-replace (whole-word)
    "Mark buffer and replace the thing. Give C-u prefix to disable whole-word match."
    (interactive (list (not current-prefix-arg)))
    (let* ((old (or (and (region-active-p)
                         (buffer-substring-no-properties (region-beginning) (region-end)))
                    (thing-at-point 'symbol)
                    (read-string "String to be replaced:")))
           escaped-old)

      (setq escaped-old (replace-regexp-in-string "\\$" "\\\\$" old))

      ;; quit the active region
      (if (region-active-p) (set-mark nil))

      (mark-whole-buffer)
      (unless (evil-visual-state-p)
        (evil-visual-state))
      (evil-ex (concat "'<,'>s/"
                       (if whole-word "\\<\\(")
                       escaped-old
                       (if whole-word "\\)\\>")
                       "/"))))

  (evil-define-text-object evil-a-defun (count &optional beg end type)
    (save-excursion
      (mark-defun)
      (evil-range (region-beginning) (region-end) type :expanded t)))

  (evil-define-text-object evil-buffer-object (count &optional beg end type)
    "Select the entire buffer as a text object."
    (evil-range (point-min) (point-max) type))

  (define-key evil-inner-text-objects-map "f" 'evil-a-defun)
  (define-key evil-outer-text-objects-map "f" 'evil-a-defun)

  (define-key evil-outer-text-objects-map "e" 'evil-buffer-object)  ;; entire
  (define-key evil-inner-text-objects-map "e" 'evil-buffer-object)  ;; entire

  (define-key evil-inner-text-objects-map "i" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "i" 'evil-a-double-quote)
  (evil-set-undo-system 'undo-redo))

(use-package expand-region
  :after evil
  :commands er/expand-region
  :init
  (evil-define-key 'visual 'global "v" #'er/expand-region)
  :config
  (setq expand-region-contract-fast-key "z"))

(use-package evil-matchit
  :after evil
  :defer 1
  :config
  (setq evilmi-shortcut "s")
  (global-evil-matchit-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode)
  (defun evil-surround-prog-mode-hook-setup ()
    "Set up surround shortcuts."
    (cond
     ((memq major-mode '(sh-mode))
      (push '(?$ . ("$(" . ")")) evil-surround-pairs-alist))
     (t
      (push '(?$ . ("${" . "}")) evil-surround-pairs-alist)))

    (when (memq major-mode '(org-mode))
      (push '(?\[ . ("[[" . "]]")) evil-surround-pairs-alist)
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))

    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

    (when (or (derived-mode-p 'js-mode)
              (memq major-mode '(typescript-mode web-mode)))
      (push '(?j . ("JSON.stringify(" . ")")) evil-surround-pairs-alist)
      (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))

    ;; generic
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup))

(use-package evil-visualstar
  :after evil
  :init
  (global-evil-visualstar-mode))

(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-key-sequence "kj")
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (evil-escape-mode 1))

(use-package evil-textobj-line
  :after evil
  :init
  (require 'evil-textobj-line))

(use-package evil-args
  :after evil
  :init
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg)

  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local evil-args-delimiters '(" ")))))


;;; (use-package evil-mark-replace :after evil)
(use-package evil-exchange
  :after evil
  :init
  (evil-exchange-install)) ;; gx gX

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (use-package evil-collection
;;   :disabled t
;;   :after evil
;;   :init
;;   (evil-collection-init))



(require-package 'general)
(general-evil-setup t)

;; (general-imap "SPC"
;;             (general-key-dispatch 'self-insert-command
;;             :timeout 0.5
;;             "/" 'my-toggle-input-method))

;; {{ use `SPC` as leader key
(defvar my-space-map-alist
  '((help-mode . emacs)
    (dired-mode . emacs)
    (magit-mode . emacs)
    (ediff-mode . emacs)
    (occur-mode . emacs)
    (grep-mode . emacs)
    (Man-mode . emacs)
    (diff-mode . emacs)
    (magit-blob-mode . emacs)
    (flymake-diagnostics-buffer-mode . emacs)
    (package-menu-mode . emacs)
    (global . (normal visual))))

(defun my-define-space-map-for-mode (mode states)
  (let ((mode-map (intern (concat (symbol-name mode) "-map"))))
    (general-define-key
     :keymaps mode-map
     :states states
     :prefix "SPC"
     :prefix-map 'space-prefix-map
     )))

(dolist (p my-space-map-alist)
  (my-define-space-map-for-mode (car p) (cdr p)))

(add-hook 'ediff-keymap-setup-hook
  (lambda ()
    (define-key ediff-mode-map (kbd "SPC") space-prefix-map)))

(general-create-definer space-map
  :keymaps 'space-prefix-map)

(space-map
  "SPC" 'execute-extended-command
  "TAB" 'mode-line-other-buffer
  "," 'evil-avy-goto-subword-1
  ";" 'comment-dwim
  "0" 'winum-select-window-0-or-10
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8
  "9" 'winum-select-window-9

  ;;= buffers
  "bb" 'consult-buffer
  "bp" 'consult-project-buffer
  "bg" 'magit-display-repository-buffer
  "bc" 'clone-indirect-buffer-other-window
  "by" 'copy-filename-of-current-buffer
  "bY" 'copy-buffer-name-without-extension
  "xb" 'ibuffer                      ; list-buffers
  "xk" 'kill-this-buffer
  "bq" 'bury-buffer
  "bR" 'revert-buffer

  ;;= code
  "cc" 'quickrun-compile-only
  "cr" 'kk/quickrun-smart ; c-u, c-u c-u
  "cm" 'compile ; make
  "cM" 'recompile
  "cp" 'kk/run-project-task
  "cs" 'async-shell-command
  "==" 'kk/format-buffer-or-region
  "cn" 'next-error
  "cp" 'previous-error
  "cf" 'first-error

  ;;= dired
  "dd" 'dirvish
  "dj" 'dired-jump
  "ds" 'dirvish-side
  "do" 'dirvish-quick-access
  ;; "df" 'dirvish-fd
  "df" 'project-find-dir
  "dr" 'my-recent-directory ;; open the dired from current file
  "dy" 'copy-directory-path-of-current-buffer

  ;;= elisp & error
  "eb" 'eval-buffer
  "ee" 'eval-expression
  "es" 'eval-last-sexp
  "ef" 'eval-defun
  "er" 'eval-region
  "el" 'flymake-show-buffer-diagnostics ;error list

  ;;= files
  "ff" 'kk/consult-fd       ; c-u
  "fs" 'kk/consult-fd-similar
  "fr" 'consult-recent-file
  "fo" 'ff-find-other-file
  "fy" 'copy-full-path-of-current-buffer
  "fY" 'copy-relative-path-of-current-file
  "xf" 'find-file
  "xs" 'save-buffer

  ;; gpt

  ;;= highlight & httpd & help
  "hk" 'describe-key
  "hc" 'describe-command
  "hv" 'describe-variable
  "hf" 'describe-function
  "hf" 'describe-function
  "hm" 'describe-mode
  "hk" 'describe-keymap
  "hs" 'describe-symbol
  "hi" 'info-apropos ; search indices of all known Info files
  "he" 'info-emacs-manual ; i(Info-index) ^(go to info directory node)

  "hh" 'kk/hl-current
  "hp" 'kk/hl-pattern ; C-u -> add pattern
  "hr" 'highlight-regexp
  "hl" 'highlight-lines-matching-regexp
  "hu" 'unhighlight-regexp ; if C-u was specified, then remove all hi-lock highlighting
  "hU" 'kk/hl-unhighlight-all

  ;; insert
  "ia" 'kk/align-dwim              ; align-regexp, input (\->\)
  "is" 'flyspell-auto-correct-word ; ispell-word
  "iy" 'consult-yank-pop           ; used frequently
  "ie" 'emoji-search
  "iu" 'insert-char ; Unicode
  "in" 'yas-new-snippet
  "ii" 'yas-insert-snippet ; yas-expand
  "ih" 'hippie-expand
  "i/" 'yas-visit-snippet-file
  ;; yas-compile-directory
  ;; "ir" 'yas-reload-all
  "il" 'yas-describe-tables
  "ic" 'aya-create
  ;; "ie" 'aya-expand
  "ir" 'kk/evil-query-replace

  ;;= jump  i/o a/e b/f d/u h/l n/p
  "jj" 'kk/swiper  ; C-u
  "jb" 'kk/consult-rg-current
  "jk" 'evil-avy-goto-line
  "ji" 'consult-imenu
  "jI" 'consult-imenu-multi
  "jm" 'consult-mark

  ;;= keyword search
  "km" 'man-follow
  "kk" 'gt-translate
  "kq" 'gt-translate-query
  "ku" 'gt-delete-render-overlays
  "ka" 'gt-record-words-as-known
  "kd" 'gt-record-words-as-unknown
  ;; "kg" 'w3m-google-search
  ;; "kd" 'w3m-search-financial-dictionary
  ;; "ka" 'w3m-java-search
  ;; "kq" 'w3m-stackoverflow-search

  ;;= language & lsp
  ;; "la" 'xref-find-apropos
  ;; "ls" 'kk/xref-find-definitions-prompt
  ;; "ll" 'xref-find-definitions ;; C-u
  ;; "lr" 'xref-find-references
  "ls" 'citre-query-jump
  "ll" 'kk/citre-jump-or-grep ;; C-u
  "lr" 'citre-jump-to-reference
  "lb" 'citre-jump-back

  "lT" 'citre-update-this-tags-file
  "lG" 'citre-global-update-database

  ;;= bookmark
  "mb" 'consult-bookmark ; bookmark-jump
  "ms" 'bookmark-set
  "ml" 'bookmark-bmenu-list

  ;; open
  "oi" (lambda () (interactive) (find-file user-init-file))
  "of" 'open-favorites
  "oe" 'eat-project
  "os" 'scratch
  "oo" 'popper-toggle
  "op" 'kk/consult-popper-buffers
  "ov" 'vertico-repeat
  "om"  'view-echo-area-messages
  "ol"  'view-lossage

  ;;= project & paste
  ;; "p" 'projectile-command-map
  "pp" 'project-switch-project
  "p!" 'project-shell-command
  "pk" 'project-kill-buffers
  "py" 'copy-project-path-of-current-buffer

  ;;= search
  "ss" 'kk/consult-rg ;; C-u
  "sd" 'kk/consult-rg-directory
  "sb" 'consult-line-multi

  ;;= toggle
  "tf" 'toggle-frame-fullscreen
  "tF" 'toggle-frame-maximized
  "tw" 'toggle-word-wrap ; visual-line-mode
  "tc" 'c-toggle-comment-style
  "tv" 'visible-mode
  "tr" 'read-only-mode
  "ts" 'flyspell-mode ; flyspell-prod-mode
  "tp" 'popper-toggle-type
  "td" 'debug-on-entry
  ;; "td" 'toggle-debug-on-quit  ; pkill -SIGUSR2 emacs
  ;; "td" 'toggle-debug-on-error

  ;;= u->gud
  "ur" 'gud-remove
  "ub" 'gud-break
  "uu" 'gud-run
  "up" 'gud-print
  "un" 'gud-next
  "us" 'gud-step
  "ui" 'gud-stepi
  "uc" 'gud-cont
  "uf" 'gud-finish

  ;;= versioning/git
  "vv" 'magit-status
  "v/" 'magit-dispatch
  "vF" 'magit-fetch
  "v." 'magit-file-dispatch ;; stage unstage blame blame-quit log magit-blame-cycle-style
  "vR" 'vc-revert
  "vr" 'diff-hl-revert-hunk
  "vs" 'diff-hl-stage-dwim
  "vp" 'diff-hl-previous-hunk
  "vn" 'diff-hl-next-hunk
  "vdd" 'diff-hl-diff-goto-hunk    ; vc-diff
  ;; "vdw" 'ediff-regions-wordwise
  ;; "vdl" 'ediff-regions-line
  "vdb" 'ediff-buffers ; first narrow, then compare
  "vdw" 'kk/ediff-buffers-wordwise ;; ediff-windows-wordwise
  "vdn" 'kk/ediff-new
  "vdg" 'kk/ediff-go
  ;; "vdg" 'magit-ediff-resolve
  ;; vc-resolve-conflicts

  ;;= windows & workspace
  "ww" 'switch-window
  "wo" 'sanityinc/toggle-delete-other-windows
  "wu" 'my-transient-winner-undo
  "nn" 'my-narrow-or-widen-dwim

  "ws" (split-window-func-with-other-buffer 'split-window-vertically)
  "wv" (split-window-func-with-other-buffer 'split-window-horizontally)
  "w==" 'balance-windows
  "w=}" 'enlarge-window-horizontally
  "w={" 'shrink-window-horizontally
  "w=^" 'enlarge-window
  "w=v" 'shrink-window

  "wh" 'windmove-swap-states-left
  "wl" 'windmove-swap-states-right
  "wk" 'windmove-swap-states-up
  "wj" 'windmove-swap-states-down

  "x0" 'delete-window
  "x1" 'delete-other-windows
  "x2" 'split-window-vertically
  "x3" 'split-window-horizontally

  "wq" 'delete-window
  "wx" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"
  "xx" 'delete-frame
  "xc" 'save-buffers-kill-terminal)
;; }}

(general-define-key
 :states '(normal visual)
 ;; "," 'evil-repeat-find-char-pinyin-reverse
 ;; ";" 'evil-repeat-find-char-pinyin
 "D" 'puni-kill-line
 ;; "zz" 'repeat
 "\\" 'evil-emacs-state
 "M-." 'xref-find-definitions           ; C-u
 "C-]" 'kk/citre-jump-or-grep
 "gh" 'evil-first-non-blank
 "gl" 'evil-end-of-line
 "]s" 'evil-next-flyspell-error
 "[s" 'evil-prev-flyspell-error
 "go" 'goto-char)

(general-define-key
 :states 'insert
 "C-e" 'move-end-of-line
 "C-k" 'kill-line
 "M-j" 'yas-insert-snippet
 ;; "M-j" 'yas-expand ; TAB
 ;; "M-j" 'aya-expand
 "C-x C-n" 'evil-complete-next-line
 "C-x C-p" 'evil-complete-previous-line)

(general-define-key
 :keymaps 'evil-ex-completion-map
 "C-d" 'delete-char
 "C-f" 'forward-char
 ;; use `my-search-evil-ex-history' to replace `evil-ex-command-window'
 "C-s" 'evil-ex-command-window
 "C-r" 'my-search-evil-ex-history
 "C-a" 'move-beginning-of-line
 "C-b" 'backward-char
 "M-p" 'previous-complete-history-element
 "M-n" 'next-complete-history-element)

(general-define-key
 :states '(normal visual)
 :keymaps '(prog-mode-map)
 "(" 'backward-up-list     ;; evil-backward-sentence-begin
 ")" 'down-list            ;; evil-forward-sentence-begin
 "[l" 'backward-list
 "]l" 'forward-list
 ;; "{" 'backward-sexp       ;; evil-backward-paragraph
 ;; "}" 'forward-sexp        ;; evil-forward-paragraph
 "]e" 'flymake-goto-next-error
 "[e" 'flymake-goto-prev-error)

;; evil-cleverparens lispyville
(general-define-key
 :states '(normal visual)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "C-]"  'find-function
 ;; "[s" 'paredit-backward-up  ;; use frequently
 ;; "]s" 'paredit-forward-down  ;; use frequently
 ;; "(" 'paredit-backward-up  ;; use frequently
 ;; ")" 'paredit-forward-up   ;; ")o" for paredit-close-round-and-newline

 ;; "F(" for 'paredit-backward
 ;; "f)" for 'paredit-forward
 ;; "F)" for 'paredit-backward-down
 ;; "f(" for 'paredit-forward-down
 ;; "{" 'paredit-backward-down
 ;; "}" 'paredit-forward-down  ;; use frequently
 "H" 'paredit-backward
 "L" 'paredit-forward

 "]>" 'paredit-forward-slurp-sexp
 "]<" 'paredit-forward-barf-sexp
 "[<" 'paredit-backward-slurp-sexp
 "[>" 'paredit-backward-barf-sexp
 ;; add ") (" to paredit-split-sexp
 "+" 'paredit-split-sexp
 ;; delete ") (" to paredit-join-sexp
 "-" 'paredit-join-sexps
 ;; work with evil-surround to wrap or raise
 "D" 'paredit-kill
 "SPC ;" 'paredit-comment-dwim)

(general-define-key
 :states '(normal visual)
 :keymaps '(c++-mode-map c-mode-map)
 "[[" 'my-cpp-backward-function
 "]]" 'my-cpp-forward-function)

(general-define-key
 :states '(normal visual)
 :keymaps 'org-mode-map
 "gh" 'outline-up-heading
 "$" 'org-end-of-line
 "^" 'org-beginning-of-line
 "<" (lambda () (interactive) (org-demote-or-promote 1))
 ">" 'org-demote-or-promote
 "TAB" 'org-cycle)

(general-define-key
 :states '(normal visual)
 :keymaps 'markdown-mode-map
 "gh" 'outline-up-heading
 "TAB" 'markdown-cycle)

(defun my/eglot-setup-keybindings ()
  "Set up keybindings for eglot."
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   :keymaps 'local
   ;; workspace
   ;; "wq" #'eglot-shutdown
   ;; "wr" #'eglot-reconnect
   ;; "ws" #'eglot
   ;; "wd" #'eglot-show-workspace-configuration

   ;; formatting
   ;; "==" #'eglot-format-buffer
   ;; "=r" #'eglot-format

   ;; goto
   "lf" #'xref-find-apropos
   "ll" #'xref-find-definitions
   "lr" #'xref-find-references
   "li" #'eglot-find-implementation
   "lt" #'eglot-find-typeDefinition
   "ld" #'eglot-find-declaration

   ;; actions
   "laq" #'eglot-code-action-quickfix
   "lar" #'eglot-code-action-rewrite
   "lai" #'eglot-code-action-inline
   "lae" #'eglot-code-action-extract
   "lao" #'eglot-code-action-organize-import
   "lar" #'eglot-rename
   ))

(add-hook 'eglot-managed-mode-hook #'my/eglot-setup-keybindings)

(provide 'init-evil)
;;; init-evil.el ends here
