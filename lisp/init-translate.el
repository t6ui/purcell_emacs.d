;;; init-translate --- Config for translate    -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gt
  :commands (gt-translate-query)
  :init
  ;; pop-to-buffer
  (setq gt-buffer-render-follow-p t)
  (setq gt-buffer-render-window-config
        '((display-buffer-reuse-window
           display-buffer-use-some-window
           display-buffer-pop-up-window)
          (inhibit-same-window . t)))
  :config

  ;; mkdir -p ~/.stardict/dic
  ;; cd ~/.stardict/dic
  ;; wget http://download.huzheng.org/zh_CN/stardict-langdao-ce-gb-2.4.2.tar.bz2
  ;; wget http://download.huzheng.org/zh_CN/stardict-langdao-ec-gb-2.4.2.tar.bz2
  ;; tar xvf stardict-langdao-ec-gb-2.4.2.tar.bz2
  ;; tar xvf stardict-langdao-ce-gb-2.4.2.tar.bz2
  ;; sdcv -l

  (add-hook 'gt-buffer-render-init-hook
            (lambda () (define-key evil-normal-state-local-map (kbd "q") 'quit-window)))
  (setq gt-buffer-prompt-after-init-function
        (lambda ()
          (when (featurep 'evil)
            (evil-insert-state))))

  (setq gt-langs '(en zh))
  (setq gt-preset-translators
        `((default . ,(gt-translator
                       :taker   (list (gt-taker :pick nil :if 'selection)
                                      (gt-taker :text 'buffer :if '(Info-mode help-mode))
                                      (gt-taker :text 'paragraph :pick 'word :pick-pred 'gt-word-fresh-p :if 'read-only)
                                      (gt-taker :text 'word))
                       :engines (list (gt-stardict-engine
                                       :dir "~/.stardict/dic"
                                       :exact t
                                       :if 'word)
                                      (gt-youdao-dict-engine :if 'word)
                                      (gt-youdao-suggest-engine :if 'word)
                                      (gt-bing-engine))
                       :render (list (gt-overlay-render :if 'read-only)
                                     (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                     (gt-buffer-render))))
          (immersive . ,(gt-translator
                         :taker (gt-taker :text 'buffer :pick 'paragraph)
                         :engines (gt-bing-engine)
                         :render (gt-overlay-render)))
          (query . ,(gt-translator
                     :taker (gt-taker :text 'point :pick nil :prompt 'buffer)
                     :engines (gt-bing-engine)
                     :render (gt-buffer-render)))
          (Text-Utility . ,(gt-text-utility
                            :taker (gt-taker :pick nil)
                            :render (gt-buffer-render)))))

  (defun gt-translate-query ()
    ""
    (interactive)
    (let ((gt-default-translator
           (cdr (assoc 'query gt-preset-translators))))
      (gt-ensure-default-translator)
      (gt-start gt-default-translator))))

(provide 'init-translate)
;;; init-translate.el ends here
