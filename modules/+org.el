(add-hook 'org-mode-hook
          (lambda()
            (local-unset-key (kbd "C-,"))
            (local-set-key (kbd "C-c h") 'org-insert-heading)
            (local-set-key (kbd "C-c o") 'org-open-at-point)
            ))


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


(setq org-log-done t)
(setq org-startup-folded "showeverything")
(setq org-directory (expand-file-name "~/.emacs.d/org"))
(setq org-agenda-files (list "~/.emacs.d/org/all.org"))

;; (global-set-key (kbd "C-c t") 'org-todo-list)
;; (global-set-key (kbd "C-c s") 'org-schedule)
;; (global-set-key (kbd "C-c a") 'org-agenda)
