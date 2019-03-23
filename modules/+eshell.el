;; Eshell
;; requires magit and all-the-icons
(require 'eshell)
(require 'magit)
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (concat (abbreviate-file-name (eshell/pwd))) 'face `(:foreground "#a991f1" :weight bold))
         (propertize " ")
         (if (magit-get-current-branch)
             (propertize (all-the-icons-octicon "git-branch")
                         'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                         'display '(raise -0.1)))
         (propertize " ")
         (if (magit-get-current-branch)
             (propertize (magit-get-current-branch) 'face `(:foreground "#7bc275" :weight bold)))
         ;;   (propertize "z" 'face `(:foreground "yellow")))
         ;; (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
         (propertize "\n" 'face `(:foreground "#7bc275"))
         (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "#7bc275" :weight bold))
         )))


(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "TERM" "xterm-256color")))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))
