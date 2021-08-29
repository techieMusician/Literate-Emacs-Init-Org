;;;;;;Modified-by: Mike Grammes <mike.grammes@gmail.com>

;;;;; DO NOT MODIFY THIS FILE BY HAND
;;    This file is maintaned at ~/emacs.d.Mike/emacs.org
;;    Make all changes there

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
(menu-bar-mode 1)            ;  Keep menubar

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
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (define-key help-map "\C-h" 'which-key-C-h-dispatch))

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

;; set up magit for git integration
(use-package magit)
;; look into force for github integration

(use-package doom-modeline
      :init (doom-modeline-mode 1)
      :custom (doom-modeline-height 15)
               (doom-modeline-bar-width 70)
               (doom-modeline-window-width-limit fill-column)
               (doom-modeline-vcs-max-length 25))

(use-package all-the-icons)
(setq auto-revert-check-vc-info t)

(use-package toc-org
   :commands toc-org-enable
   :init (add-hook 'org-mode-hook 'toc-org-enable))

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
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  :config
  (setq org-ellipsis " ▾")
                                        ; (setq org-ellipsis " ^")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'note)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/org/inbox.org"
          "~/org/projects.org"
          "~/org/homeserver.org"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n@/!)" "|" "DONE(d@/!)")))
  (setq org-refile-targets
        '(("archives.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)
          ("projects.org" :maxlevel . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-outline-path-complete-in-steps nil)
  ;; Save Org buffers after refiling:
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; add tags
  (setq org-tag-alist
        '((:startgroup)
                                        ;put mutually exclusize tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@music" . ?M)
          ("idea" . ?i)
          ("note" . ?n)))
  ;; Configure custom agenda views

  ;; Configure org-capture templates
  (setq org-capture-templates
    `(("t" "Task" entry (file "~/org/inbox.org")
       "* TODO %?\n  %U\n  %i" :empty-lines 1)))
  (require 'org-protocol)
  (efs/org-font-setup))

;;;; Setup org-roam
(use-package org-roam
  :init
;; remove warning for org-roam-v2
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (file-truename "~/org/"))
  ;; Configure org-roam-capture templates
  (setq org-roam-capture-templates
    `(("d" "default" plain (function org-roam-capture--get-point)
       "%?"
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+title: ${title}\n"
       :unnarrowed t)
      ("t" "Task" plain (function org-roam-capture--get-point)
       "* TODO %?\n %U\n %i" :empty-lines 1
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head "#+title: ${title}\n"
       :unnarrowed t)))

  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode t))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (python . t)
   (shell . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'org-babel-tangle
                          :append :local)))
