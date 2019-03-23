;; Solidity

;; solidity-mode
(use-package solidity-mode
  :ensure t
  :defer 10
  :config
  (add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode)))
