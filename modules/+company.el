;; Company
(use-package company
  :config
  (setq company-backends
        '(company-elisp
          company-semantic
          company-capf
          (company-dabbrev-code company-gtags company-etags
                                company-keywords)
          company-files
          company-dabbrev))
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay .2)
  (setq company-dabbrev-other-buffers t)
  (setq company-auto-complete nil)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-code-ignore-case t)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))
;; (add-hook 'after-init-hook 'global-company-mode))
