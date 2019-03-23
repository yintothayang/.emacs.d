;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :init (setq typescript-indent-level 2)
  :hook (('typescript-mode . 'highlight-symbol-mode)
	       ('typescript-mode . 'highlight-indent-guides-mode)))


;; (add-hook 'typescript-mode-hook 'highlight-indentation-mode)
;; (add-hook 'typescript-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'typescript-mode-hook 'flycheck-mode)
;; (defun colorize-compilation-buffer ()
;;   (ansi-color-apply-on-region compilation-filter-start (point-max)))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
