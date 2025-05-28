;;; init.el --- Where all the magic begins

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)

(setq ns-auto-hide-menu-bar t)
;; (set-frame-position nil 0 -24)
;; (set-frame-size nil (display-pixel-width) (display-pixel-height) t)

(add-to-list 'frameset-filter-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(undecorated . t))

;; (load-theme 'doom-miramare)
;; (load-theme 'doom-flatwhite)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(menu-bar-mode 1)
(load-theme 'tango-dark)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(column-number-mode)
(global-display-line-numbers-mode -1)
(global-display-line-numbers-mode 1)

(set-face-attribute 'default nil :height 160)
(set-face-attribute 'default nil :font "FuraMono Nerd Font Mono")

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))
;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; don't know how to set bindings without pacakge
;; so all general bindings are here lmao...
(use-package command-log-mode
  :bind (("C-c t" . load-theme)
	 ("C-c ." . eval-last-sexp)))

(use-package emacs
	:hook
	(js-mode . lsp))


(defun proj ()
	"Opens find-file in personal projects folder."
	(interactive)
	(let ((default-directory "~/Downloads/projects/"))
		  (call-interactively 'find-file)))


(let ((percent (/ (float (point))
				  (float (point-max)))))
	  (concat "cursor is "
			  (number-to-string
			  (truncate (* percent 100)))
			  "% through"))



(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 25)
	   (doom-modeline-icon nil)
	   (doom-modeline-lsp-icon nil)
	   (doom-modeline-lsp nil)
	   (doom-modeline-project-name t)
	   (doom-modeline-time t)
	   (doom-modeline-buffer-encoding nil)
	   (doom-modeline-indent-info t)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package flycheck) 
(global-flycheck-mode)


(use-package evil

  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :after evil
  :config
  
  (general-create-definer efs/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")

  (efs/leader-keys
    ; Finding stuffy
    "f"  '(:ignore t :which-key "find")
    "ff"  '(find-file :which-key "find files")
    "fb"  '(switch-to-buffer :which-key "find buffer")
    ; killing windows and buffers
    "k"  '(:ignore t :which-key "kill")
    "kw"  '(evil-window-delete :which-key "kill window")
    "kb"  '(kill-buffer :which-key "kill buffer")))

(setq-default tab-width 2)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package doom-themes)

(use-package org
  :config
  (setq org-agenda-files
	'("~/.config/emacs/org-files/tasks.org"
	  "~/.config/emacs/org-files/birthdays.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  )

(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)


;; Further reading: https://protesilaos.com/emacs/dotemacs#h:cff33514-d3ac-4c16-a889-ea39d7346dc5
(use-package vertico
  :config
  (setq vertico-resize nil)
  (setq vertico-count 8)
  (vertico-mode 1))


;; Further reading: https://protesilaos.com/emacs/dotemacs#h:bd3f7a1d-a53d-4d3e-860e-25c5b35d8e7e
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))

;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:22e97b4c-d88d-4deb-9ab3-f80631f9ff1d
(use-package consult
	:ensure t)

(use-package spacious-padding)

(setq spacious-padding-widths
		'( :internal-border-width 12
				:header-line-width 4
				:mode-line-width 6
				:tab-width 2
				:right-divider-width 20
				:scroll-bar-width 2
				:fringe-width 22))

(spacious-padding-mode 1)

(use-package magit)
(use-package pbcopy)
