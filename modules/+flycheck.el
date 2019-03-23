;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (use-package flycheck-posframe
;;   :after flycheck
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
