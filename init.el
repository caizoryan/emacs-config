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
(setf make-backup-files nil)
(menu-bar-mode 1)
(load-theme 'tango-dark)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(column-number-mode)
(global-display-line-numbers-mode -1)
(global-display-line-numbers-mode 1)


(set-face-attribute 'default nil :font "FuraMono Nerd Font Mono")
(set-face-attribute 'default nil :height 160)

(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t))) 

(setq auto-save-default nil)

(setq mac-command-modifier 'meta)
;; (global-set-key (kbd "S-d") nil)
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

(global-set-key (kbd "M-d") nil)
(global-set-key (kbd "M-D") nil)


(use-package command-log-mode)
(use-package dired-filter)

(use-package request)

;; (request "https://api.are.na/v2/channels/list-are-na-api-possibilities?per=100"
;; 	:parser 'json-read
;; 	:success (cl-function
;; 						(lambda (&key data &allow-other-keys)
;; 						(let (temp-buffer count)
;; 							(setq count 0)
;; 							(setq temp-buffer (if-let
;; 														((buffer (get-buffer  "BUFFER")))
;; 														(switch-to-buffer buffer)
;; 													(generate-new-buffer "BUFFER")))
;; 							(switch-to-buffer "BUFFER")
;; 							(delete-region (point-min) (point-max))
;; 							(princ (format "id: %S" (alist-get 'id data)) temp-buffer)
;; 							(princ "\n" temp-buffer)
;; 							(princ (format "slug: %S" (alist-get 'slug data)) temp-buffer)
;; 							(princ "\n" temp-buffer)
;; 							(seq-doseq (x (alist-get 'contents data))
;; 								(setq count (+ count 1))
;; 								(if (< count 3)
;; 										(posframe-show-string (alist-get 'content x) (cons 50 (* count 30))))
;; 								(princ (format "b[id]: %S" (alist-get 'id x)) temp-buffer)
;; 								(princ "\n" temp-buffer)
;; 								)

;; 							(princ (format "length: %d" count) temp-buffer)
;; 							)))) 

;; (json-parse-string "[{\"hello\": \"hello\"}, {\"world\": \"world\"}]")

(use-package posframe)

(defun tasks ()
	(interactive)

	(let (dawg lineNum)
		(setq dawg (if-let
									 ((buffer (get-buffer  "*TASKS*")))
									 (switch-to-buffer buffer)
								 (generate-new-buffer "*TASKS*")))

		(setq lineNum 0)

		(dolist (line (split-string
									 (buffer-substring-no-properties
										(point-min)
										(point-max)) "\n"))

		(setq lineNum (+ lineNum 1))

		(cond ((string-match-p (regexp-quote "- \[ ]") line)
					 (princ (concat (number-to-string lineNum) ": " line) dawg)
					 (princ "\n" dawg)))
		)
	(display-buffer dawg)))

(defun posframe-show-string (str &optional pos)
	(interactive "sEnter to show:")
	(message str)
	(posframe-show " *my-posframe-buffer*"
                 :string str
								 :width 40
								 :height 20
								 :border-width 2
								 :border-color "red"
                 :position (if (eq pos nil) (point) (progn (message "using pos") pos))
								 ))


(defun posframe-show-buffer (buffer)
	(posframe-show buffer
								 :width 50
								 :height 15 
								 :border-width 2
								 :border-color "red"
                 :position (point)))

(defun kill-current-buffer ()
	(interactive)
	(evil-window-delete))

(defun server (port)
	(interactive "sPort: ")
	(async-shell-command
	 (concat "python3 -m http.server " port)
	 (generate-new-buffer
		(concat "*(server" port ")*"))
	 (browse-url (concat  "http://localhost:" port))
	 ))


(use-package pdf-tools)

;; don't know how to set bindings without pacakge
;; so all general bindings are here lmao...
(use-package emacs
	:hook
	(js-mode . lsp)
  (prog-mode . hs-minor-mode)
  (prog-mode . electric-pair-mode)
	(org-mode . visual-line-mode)
	(org-mode . org-indent-mode)

	:bind (("C-c t" . load-theme)
					("C-c ." . eval-last-sexp)
					("C-x C-b" . ibuffer)
					("C-x f" . ffap)
					("C-x C-o" . find-file-other-window)
					("C-c s" . consult-imenu)
					("M-d" . evil-mc-make-and-goto-next-match)
					("M-f" . find-file)
					("M-D" . evil-mc-undo-all-cursors)
					("M-u" . evil-mc-undo-last-added-cursor)
					("M--" . text-scale-decrease)
					("M-=" . text-scale-increase)
					("M-w" . kill-current-buffer)

					("M-l" . evil-window-right)
					("M-h" . evil-window-left)
					("M-j" . evil-window-down)
					("M-k" . evil-window-top)
					("M-/" . evil-window-split)
					("M-|" . evil-window-vsplit))
	)

(defun center-window ()
	(interactive)
  (let ((margin-size (/ (- (frame-width) 70) 2)))
    (set-window-margins nil margin-size margin-size)))

(defun writing-mode ()
	(interactive)
	(center-window)
	(visual-line-mode)
	(display-line-numbers-mode)
	(variable-pitch-mode))
	
(defun reset-margins ()
	(interactive)
	(set-window-margins nil 0 0))

(defun search-style-css ()
	(interactive)
	(let (buf))
	(setq buf (buffer-substring-no-properties (region-beginning) (region-end)))
	(dolist (x (projectile-project-buffers))
		(if (string-match-p (regexp-quote "style.css") (buffer-file-name x))
				(progn
					(switch-to-buffer-other-window x)
					(consult-line buf))
			)))

(defun proj ()
	"Opens find-file in personal projects folder."
	(interactive)
	(let ((default-directory "~/Downloads/projects/"))
		  (call-interactively 'find-file)))

(use-package geiser :ensure t)
(use-package geiser-racket :ensure t)


;; MARKDOWN MODE
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
							("M-RET" . markdown-do)
							("C-c l = =" . markdown-table-align)
							("M-<up>" . markdown-move-up)
							("M-<down>" . markdown-move-down)
							("M-p" . projectile-switch-project)
							))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (
					 (doom-modeline-height 25)
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
  (setq evil-shift-width 2)

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
    "ff"  '(project-find-file :which-key "find project files")
    "fb"  '(consult-project-buffer :which-key "find project buffer")

    "fF"  '(find-file :which-key "find files")
    "fB"  '(switch-to-buffer :which-key "find buffer")
    ; killing windows and buffers
    "k"  '(:ignore t :which-key "kill")
    "kw"  '(evil-window-delete :which-key "kill window")
    "kb"  '(kill-buffer :which-key "kill buffer")))

(setq-default tab-width 2)

(use-package evil-mc
	:init
	(global-evil-mc-mode 1))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)

	:bind (("M-r" . lsp-find-references))
	)

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
	  "~/.config/emacs/org-files/birthdays.org"
	  "~/notes/todo.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'nil)
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▾" org-hide-emphasis-markers t))

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
(add-hook 'dired-mode-hook 'dired-omit-mode)

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

(use-package projectile
	:ensure t
	:config (projectile-mode +1)
	:bind (("C-x p F" . projectile-switch-project)
				 ("M-p" . projectile-switch-project)))

(setq projectile-project-search-path '("~/Downloads/projects/"))

(use-package spacious-padding)

(setq spacious-padding-widths
		'(  :right-divider-width 20
				:fringe-width 44))

(spacious-padding-mode 1)

(use-package magit)
(use-package pbcopy)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-indent-offset 2)
 '(custom-safe-themes
	 '("0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
		 default))
 '(js-indent-level 2)
 '(line-spacing 0.3)
 '(package-selected-packages
	 '(command-log-mode company-box consult dired-filter doom-modeline
											doom-themes evil-collection evil-mc flycheck
											geiser geiser-mit geiser-racket general helpful
											ivy-rich keycast lsp-mode lsp-scheme magit
											marginalia multiple-cursors orderless
											org-bullets parinfer-rust-mode pbcopy pdf-tools
											posframe projectile rainbow-delimiters request
											spacious-padding typescript-mode vertico
											volatile-highlights vterm which-key)))


(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :background "#f1ddf1" :foreground "#614c61" :weight thin :family "Gap Sans"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.8))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 2.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 2.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-link-face ((t (:inherit link :background "wheat1" :foreground "black"))))
 '(markdown-list-face ((t (:inherit markdown-markup-face :family "Hermit"))))
 '(markdown-markup-face ((t (:inherit shadow :slant normal :weight normal :family "PP Fraktion Mono"))))
 '(markdown-table-face ((t (:inherit markdown-code-face :family "RobotoMono Nerd Font Mono"))))
 '(variable-pitch ((t (:family "ABC Camera Plain Variable Unlicensed Trial"))))
 '(variable-pitch-text ((t (:inherit variable-pitch :height 1.05)))))
