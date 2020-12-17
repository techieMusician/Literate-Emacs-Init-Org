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

;; configure counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; ivyrich is an ivy extenstion that gives extra info about commands
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; add rainbow parenthesis to programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; add which-key: a panal pop to tell what keys match
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; helpful adds more useful information with helpfiles
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(forge magit counsel-projectile all-the-icons general doom-themes helpful ivy-rich counsel ivy command-log-mode commang-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
