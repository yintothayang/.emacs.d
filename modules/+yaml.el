;; YAML
(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook (('yaml-mode . 'highlight-indent-guides-mode)))
