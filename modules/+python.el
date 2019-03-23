;; Python
(use-package virtualenvwrapper)

;; (use-package python-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;   (add-hook 'anaconda-mode-hook 'flycheck-mode)
;;   ;; (add-hook 'python-mode-hook 'lsp)
;;   )
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-hook 'python-mode-hook 'flycheck-mode)



;; (use-package lsp-python
;;   :ensure t
;;   :requires lsp-mode
;;   :config
;;   (add-hook 'python-mode-hook #'lsp-python-enable))
