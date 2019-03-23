;; Eglot
(use-package eglot
  :config
  ;; (add-to-list 'eglot-server-programs '(typescript-mode . ("javascript-typescript-stdio")))
  ;; (add-to-list 'eglot-server-programs '(javascript-mode . ("javascript-typescript-stdio")))
  (add-hook 'js2-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure))
