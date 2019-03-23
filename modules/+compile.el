;; Compile

;; auto-compile
(use-package auto-compile
  :defines auto-compile
  :ensure t
  :init
  (setq load-prefer-newer t)
  :config
  (add-to-list 'load-path "~/.emacs/modules")
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))
