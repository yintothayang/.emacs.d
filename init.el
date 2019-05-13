;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
(defun tangle-init ()
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "README.org")))
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el"))
      (load-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(setq gc-cons-threshold #x40000000)

(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

(progn ;     startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (tooltip-mode 0))

(progn ;    `borg'
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
  :defer t
  :bind (("C-x m"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

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

(use-package projectile
  :config
  (setq projectile-enable-caching t)
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

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :requires ivy
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (ivy-rich-mode 1))

;; projectile
;; Company
(use-package company
  :config
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           )
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

;; LSP mode
(use-package lsp-mode)
(use-package company-lsp)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Eshell
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
            (define-key eshell-mode-map (kbd "C-@") #'eshell-down)))


;; (defvar-local eshell-hist-dirs nil)
;; (defvar-local eshell-hist-index 0)

;; (defun etc-eshell-update-hist-dir ()
;;   ;; prevent "cd /tmp" over and over from making new entries
;;   (when (not (equal (car (last eshell-hist-dirs)) (eshell/pwd)))
;;     (push (eshell/pwd) eshell-hist-dirs)))

;; (add-hook 'eshell-directory-change-hook #'etc-eshell-update-hist-dir)

;; (defun eshell-forward (n)
;;   (unless eshell-hist-dirs
;;     (user-error "eshell-hist-dirs is empty, cd a few times"))
;;   (let ((dirs eshell-hist-dirs))
;;     (prog1 (eshell/cd (nth (setq eshell-hist-index
;;                                  ;; ensure we don't go outside list bounds
;;                                  (if (> n 0)
;;                                      (min (- (length eshell-hist-dirs) 1) (+ eshell-hist-index n))
;;                                    (max 0 (+ eshell-hist-index n))))
;;                            dirs))
;;       (setq eshell-hist-dirs dirs))))

;; (defun eshell/b ()
;;   (eshell-forward 1))

;; (defun eshell/f ()
;;   (eshell-forward -1))

;; (defun etc-eshell-mode-hook ()
;;   ;; make sure starting directory is in history
;;   (push (eshell/pwd) eshell-hist-dirs))

;; (add-hook 'eshell-mode-hook #'etc-eshell-mode-hook)

;; Kubernetes
(use-package kubernetes
  :commands (kubernetes-overview))

;; Python
;; (use-package virtualenvwrapper)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))


;; Javascript
(setenv "NODE_PATH"
        (concat
         "/home/yin/.node/lib/node_modules" ":"
         (getenv "NODE_PATH")
         )
        )

(setq js-indent-level 2)
(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2)
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning nil)
  :hook (('js2-mode . 'highlight-symbol-mode)
         ('js2-mode . 'highlight-indent-guides-mode)))

;; Typescript
(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :init (setq typescript-indent-level 2)
  :hook (('typescript-mode . 'highlight-symbol-mode)
         ('typescript-mode . 'highlight-indent-guides-mode)
         ('typescript-mode . 'flycheck-mode)
         ;; ('typescript-mode .  #'lsp)
         ('typescript-mode . 'subword-mode)))

;; SQL
;; (setq sql-postgres-login-params (append sql-mysql-login-params '(port)))
(setq sql-connection-alist
      '((redshift-gs_prod (sql-product 'postgres)
                          (sql-port 5439)
                          (sql-server "gamesight.cixsp8xnn5rk.us-west-2.redshift.amazonaws.com")
                          (sql-user "gs_prod")
                          (sql-database "gamesight_prod"))))

;; Markdown
(use-package markdown-mode
  :mode "\\.ts\\'")


;; Org-Mode
(use-package ob-rust)
(use-package org-yaml)
(use-package ob-typescript)
(use-package ob-restclient)
(use-package gnuplot)
(use-package gnuplot-mode)
(use-package htmlize)
(use-package org-bullets)
(use-package ox-gfm)

(setq org-startup-folded 'showall)

(add-hook 'org-mode-hook 'org-bullets-mode)
(url-handler-mode 1)

(setq org-confirm-babel-evaluate nil)
(setq org-startup-with-inline-images t)
(setq org-default-notes-file "~/notes.org")

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "M-h") nil)
  (define-key org-mode-map (kbd "<C-tab>") 'org-global-cycle))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sql . t)
   (js . t)
   (rust . t)
   (typescript . t)
   (gnuplot . t)
   (ditaa . t)
   (latex . t)
   (shell . t)
   (restclient .t)
   (R . t)))

;; LaTex
(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)

(setq org-latex-listings-options '(("breaklines" "true")))

(setq initial-buffer-choice t)
(setq initial-buffer-choice (concat user-emacs-directory "notes.org"))


;; ;; TRAMP
;; (defconst my-tramp-prompt-regexp "Verification code: ")

;; ;; (setq verification-code (read-string "Verification code: "))

;; (defun my-tramp-action (proc vec)
;;   (save-window-excursion
;;     (with-current-buffer (tramp-get-connection-buffer vec)
;;       (message "1")
;;       (tramp-message vec 6 "\n%s" (buffer-string))
;;       (message "2")
;;       (tramp-send-string vec "390244")
;;       (message "3")
;;       )))

;; (setq tramp-actions-before-shell nil)
;; (add-to-list 'tramp-actions-before-shell
;;              '(my-tramp-prompt-regexp my-tramp-action))

;; (defadvice sql-mysql (around sql-mysql-around activate)
;;   "SSH to linux, then connect"
;;   (let ((default-directory "/ssh:gsjumpbox:"))
;;     ad-do-it))

;; Pug
(use-package pug-mode
  :defer t
  :config
  (setq pug-tab-width 2))

;; Stylus
(use-package sws-mode
  :defer t)

;; Vue
(use-package mmm-mode
  :defer t
  :config
  (setq mmm-submode-decoration-level 0))

(use-package vue-mode
  :defer t
  :requires mmm-mode
  :mode "\\.vue\\'"
  :hook (('vue-mode . 'highlight-symbol-mode)
         ('vue-mode . 'highlight-indent-guides-mode)
         ('vue-mode . 'flycheck-mode)))

;; YAML
(use-package yaml-mode
  :mode "\\.yaml\\'"
  :hook (('yaml-mode . 'highlight-indent-guides-mode)))

;; CSV
(use-package csv-mode
  :mode "\\.csv\\'")

;; UI
;; Full screen
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Font
(set-face-attribute 'default nil :height 134)
(set-frame-font "Office Code Pro")

;; Don't truncate lines
(setq-default truncate-lines t)
(setq tab-width 2)

;; Needed for hi-dpi scrolling
(pixel-scroll-mode)

;; Icons
;; Must install fonts ->  M-x all-the-icons-install-fonts
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Display Line Numbers
(setq-default display-line-numbers t)

;; Delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; hl-line
(global-hl-line-mode 1)

;; indent
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq default-tab-width 2)

;; Shutup
(setq ring-bell-function 'ignore)

;; smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; indent
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode t))

;; ;; (use-package dimmer
;; ;;   :config
;; ;;   (dimmer-mode 1))

;; Doom modeline
(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  :hook
  (after-init . doom-modeline-mode))


(use-package git-gutter
  :config
  (global-git-gutter-mode t))

(use-package highlight-symbol
  :init
  (setq highlight-symbol-idle-delay .2))

;; https://github.com/DarthFennec/highlight-indent-guides
;; (use-package highlight-indent-guides
;;   :config
;;   (setq highlight-indent-guides-method 'character)
;;   (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; Theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-one-light t)
  (doom-themes-org-config))

;; Keybindings
(if (= (display-pixel-width) 2560)
    (progn
      (message "small screen")
      (setq x-meta-keysym 'meta)
      (setq x-super-keysym 'super))
  (progn
    (message "big screen")
    (setq x-meta-keysym 'super)
    (setq x-super-keysym 'meta)))

;; expand-region
(use-package expand-region
  :config
  (global-set-key (kbd "C-o") 'er/expand-region))

;; ;; Slack
;; (use-package alert)
;; (use-package circe)
;; (use-package emojify)
;; (use-package oauth2)
;; (use-package request)
;; (use-package websocket)
;; (use-package slack
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify nil) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config
;;   (slack-register-team
;;    :name "Innervate"
;;    :default t
;;    :client-id "92edb89a-1556557059.187"
;;    :client-secret ""
;;    :token "xoxs-2151853922-3973305712-477415368855-b2464de6b77a5d12740d130bdfd8bd6cd78e38a1629861d79f796db3fd1cd77f"
;;    :subscribed-channels '(test-rename rrrrr)
;;    :full-and-display-names t))

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

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

(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'hippie-expand)

(defalias 'yes-or-no-p 'y-or-n-p)
(fset 'yes-or-no-p 'y-or-n-p)

(use-package counsel-spotify
  :config
  (setq counsel-spotify-client-id "c490bbbcd29a44f2ac727f5fbfed86a5")
  (setq counsel-spotify-client-secret "8a64340b996145868a65bee52ed06271"))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files
