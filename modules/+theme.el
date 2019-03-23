;; Theme

;; Theme List
;; doom-one
;; doom-one-light
;; doom-vibrant
;; doom-city-lights
;; doom-dracula
;; doom-molokai
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))
