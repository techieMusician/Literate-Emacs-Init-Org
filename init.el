
;;;;;;Modified-by: Mike Grammes <mike.grammes@gmail.com>

;;;;;This is my emacs config following youtube tut Emacs from Scratch
;;;;;I will be maintiaing this allongside a prelude config so I can decide
;;;;;Which I prefer. This is a work in progress

;;; Some of this will be added into an org file for literate programming.

;; use ln -s .emacs.d.Mike .emacs.d to create a soft link for this config

;; Inhibit annoying start message
(setq inhibit-startup-message t)


;; Remove other UI crutches, be a big boy!
(scroll-bar-mode -1)         ; Disable visable Scrollbar
(tool-bar-mode -1)           ; Disable toolbar
(tooltip-mode -1)            ; Disable tooltips
(set-fringe-mode 10)         ; Give space around emacs
(menu-bar-mode -1)           ; Get rid of menubar

;; Set a pretty theme
;(load-theme 'doom-dracula)


;; add line number mode globally
(global-display-line-numbers-mode t)

;; Disable line numebers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-command-with-editor-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;;;; Font Configuration
(defvar runemacs/default-font-size 100)
(set-face-attribute 'default nil :font "Fira Code Retina" :height runemacs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 115 :weight 'regular)

;;; Manage packages with use-package

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Doom themes
(use-package doom-themes
  :init (load-theme 'doom-tomorrow-night t))

(use-package all-the-icons)


;; command log mode makes a nice window to the side for information
(use-package command-log-mode)


;; Set up IVY for completion
;; ran M-x install-package cousel to get counsel and swiper
(use-package ivy
  :diminish
  :bind (("C-s" . swiper))
  :config
  (ivy-mode 1))

;; ivyrich is an ivy extenstion that gives extra info about commands
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; configure counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; add rainbow parenthesis to programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; add which-key: a panal pop to tell what keys match
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (define-key help-map "\C-h" 'which-key-C-h-dispatch))

;; helpful adds more useful information with helpfiles
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-c C-d" . helpful-at-point)
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; set up projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-competion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/github")
    (setq projectile-project-search-path '("~/github")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; set up magit for git integration
(use-package magit)
;; look into force for github integration

;;;; ORGMODE SETTINGS

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))


(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
;;; Setu orgmode
(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
					; (setq org-ellipsis " ^")
  (require 'org-protocol)
  (efs/org-font-setup))

;;;; Setup org-roam
(use-package org-roam
  :config
  (setq org-roam-directory "~/org")
  (add-hook 'after-init-hook 'org-roam-mode)
  (require 'org-roam-protocol))
  
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode t))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;DON'T CHANGE THIS
;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-roam-protocol unicode-fonts org-roam visual-fill-column org-bullets which-key use-package rainbow-delimiters ivy-rich helpful general forge doom-themes counsel-projectile command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
