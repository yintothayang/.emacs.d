;;; -*- lexical-binding: t -*-
(defun tangle-init ()
(interactive)
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "README.org")))
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el"))
      (load-file (concat user-emacs-directory "init.el")))))

(setq gc-cons-threshold #x40000000)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                       (message "Garbage Collector has run for %.06fsec"
                       (k-time (garbage-collect))))))

(progn
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq load-prefer-newer t)
  (setq auto-window-vscroll nil)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (tooltip-mode 0))

(progn
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'reveal-mode)
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package magit
  :bind ("C-x m"   . magit-status)
  :config)

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(use-package smex)

(use-package flycheck
:config
  (global-flycheck-mode t))

(use-package ivy
  :requires smex
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order)))
  (setq ivy-initial-inputs-alist nil)
  (setq projectile-completion-system 'ivy)
  (setq counsel-async-filter-update-time 10000)
  (setq ivy-dynamic-exhibit-delay-ms 20)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-t") 'complete-symbol)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

  ;; https://github.com/Yevgnen/ivy-rich
  (use-package ivy-rich
    :requires ivy
    :config
    (setq ivy-format-function #'ivy-format-function-line)
    (ivy-rich-mode 1))

(use-package projectile
  :config
  ;; (setq projectile-enable-caching t)
  (setq projectile-require-project-root nil)
  (setq projectile-globally-ignored-directories
        (append '(
                  ".git"
                  ".svn"
                  "out"
                  "repl"
                  "target"
                  "venv"
                  "node_modules"
                  "dist"
                  "lib"
                  )
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files
        (append '(
                  ".DS_Store"
                  "*.gz"
                  "*.pyc"
                  "*.jar"
                  "*.tar.gz"
                  "*.tgz"
                  "*.zip"
                  "*.elc"
                  "*-autoloads.el"
                  )
                projectile-globally-ignored-files))
  (projectile-mode))

(use-package counsel-projectile
  :defines personal-keybindings
  :bind ("C-x f" . counsel-projectile-find-file)
  :bind ("C-x p" . projectile-switch-open-project))

(use-package company
  :config
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords)       ; keywords
          (company-abbrev company-dabbrev company-ctags company-capf)
          ))
  ;; (setq company-backends
  ;;       '(company-elisp
  ;;         company-semantic
  ;;         company-capf
  ;;         (company-dabbrev-code company-gtags company-etags
  ;;                               company-keywords)
  ;;         company-files
  ;;         company-dabbrev))
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

(use-package flymake
  :config)

(use-package eglot
  :config)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package polymode
  :config
  (define-key polymode-mode-map (kbd "M-n") nil)
  )

(define-hostmode poly-zero-hostmode
  :mode 'typescript-mode)

(define-innermode poly-zero-pug-innermode
  :mode 'pug-mode
  :head-matcher "<template lang=\"pug\">"
  :tail-matcher "</template>"
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-zero-stylus-innermode
  :mode 'stylus-mode
  :head-matcher "lang=\"stylus\">"
  :tail-matcher "</style>"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-zero-mode
  :hostmode 'poly-zero-hostmode
  :innermodes '(poly-zero-pug-innermode
                poly-zero-stylus-innermode
                ))

(with-eval-after-load 'poly-zero-mode
  (define-key org-mode-map (kbd "M-n") 'end-of-buffer))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . poly-zero-mode))

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

(defun eshell-up ()
  (interactive)
  (with-current-buffer "*eshell*"
    (eshell-return-to-prompt)
    (insert "cd ..")
    (eshell-send-input)))

(defun eshell-down ()
  (interactive)
  (with-current-buffer "*eshell*"
    (eshell-return-to-prompt)
    (insert "cd -")
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-key eshell-mode-map (kbd "C-/") #'eshell-up)
            (define-key eshell-mode-map (kbd "C-@") #'eshell-down)
            (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
            ))

(add-hook 'emacs-lisp-mode-hook 'company-mode)

(setenv "NODE_PATH"
  (concat "/home/yin/.node/lib/node_modules" ":" (getenv "NODE_PATH")))

(setq js-indent-level 2)
(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning nil)
  :hook
  ;; ('js2-mode . 'company-mode)
  ('js2-mode . 'highlight-symbol-mode)
  ('js2-mode . 'eglot-ensure))

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :init (setq typescript-indent-level 2)
  :hook (('typescript-mode . 'highlight-symbol-mode)
   ;; ('typescript-mode . 'highlight-indent-guides-mode)
   ;; ('typescript-mode . 'flycheck-mode)
   ;; ('typescript-mode .  #'lsp)
   ('typescript-mode . 'eglot-ensure)
   ;; ('typescript-mode . 'company-mode)
   ('typescript-mode . 'subword-mode)))

;; (setq sql-postgres-login-params (append sql-mysql-login-params '(port)))
(setq sql-connection-alist
'((redshift-gs_prod (sql-product 'postgres)
        (sql-port 5439)
        (sql-server "gamesight.cixsp8xnn5rk.us-west-2.redshift.amazonaws.com")
        (sql-user "gs_prod")
        (sql-database "gamesight_prod"))))

(use-package markdown-mode
  :mode "\\.md\\'")

(require 'ob-plantuml)
(setq org-plantuml-jar-path
      (expand-file-name "~/.plantuml/plantuml.jar"))

(use-package org-bullets)
(use-package org-yaml)
(use-package ob-typescript)
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package ox-gfm)
(use-package ob-async)

(setq org-startup-folded 'showall)
(setq org-export-babel-evaluate nil)

(add-hook 'org-mode-hook 'org-bullets-mode)
(url-handler-mode 1)

(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)
(setq org-default-notes-file "~/notes.org")

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "M-h") nil)
  (define-key org-mode-map (kbd "C-/") 'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-@") 'widen)
  (define-key org-mode-map (kbd "<C-tab>") 'org-global-cycle))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
 (sql . t)
 (js . t)
 (typescript . t)
 (gnuplot . t)
 (ditaa . t)
 (latex . t)
 (shell . t)

 (R . t)))


;; LaTex
(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)

(setq org-latex-listings-options '(("breaklines" "true")))

(setq initial-buffer-choice t)
(setq initial-buffer-choice (concat user-emacs-directory "notes.org"))

(use-package pug-mode
  :config
  (setq pug-tab-width 2))

(use-package sws-mode)

(use-package mmm-mode
  :config
  (setq mmm-submode-decoration-level 0))

(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook (('yaml-mode . 'highlight-indent-guides-mode)))

(use-package csv-mode
  :mode "\\.csv\\'")

(if (= (display-pixel-width) 2560)
    (progn
      (message "small screen")
      (set-face-attribute 'default nil :height 134)
      (setq x-meta-keysym 'meta)
      (setq x-super-keysym 'super))
  (progn
    (message "big screen")
    (set-face-attribute 'default nil :height 115)
    (setq x-meta-keysym 'super)
    (setq x-super-keysym 'meta)))

 (set-frame-parameter nil 'fullscreen 'fullboth)

(set-frame-font "Office Code Pro")

(setq-default truncate-lines t)

(pixel-scroll-mode)

(use-package all-the-icons)
(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(setq-default display-line-numbers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-hl-line-mode 1)

;; (use-package aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode t))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq default-tab-width 2)

(setq ring-bell-function 'ignore)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-lsp t)
  :hook
  (after-init . doom-modeline-mode))

(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package highlight-symbol
  :init
  (setq highlight-symbol-idle-delay .2))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
  doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-one-light t)
  (doom-themes-org-config))

(use-package expand-region
  :config
  (global-set-key (kbd "C-o") 'er/expand-region))

(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-r") 'redo)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-.") 'previous-buffer)
(global-set-key (kbd "C-x 1") 'split-window-right)

(global-set-key (kbd "M-p") 'beginning-of-buffer)
(global-set-key (kbd "M-n") 'end-of-buffer)

(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "M-c") 'kill-ring-save)

(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'hippie-expand)

(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files
