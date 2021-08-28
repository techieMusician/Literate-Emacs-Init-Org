
;;;;;;Modified-by: Mike Grammes <mike.grammes@gmail.com>

;;;;;This is my emacs config following youtube tut Emacs from Scratch
;;;;;I will be maintiaing this allongside a prelude config so I can decide
;;;;;Which I prefer. This is a work in progress

;;; Some of this will be added into an org file for literate programming.

;; use ln -s .emacs.d.Mike .emacs.d to create a soft link for this config

(org-babel-load-file "~/.emacs.d/emacs.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(persistent-soft zetteldeft deft doom-modeline org-roam-protocol unicode-fonts org-roam visual-fill-column org-bullets which-key use-package rainbow-delimiters ivy-rich helpful general forge doom-themes counsel-projectile command-log-mode all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
