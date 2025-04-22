;;--------- Package Management
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;--------- Custom file settings
;; Set location for custom.el information
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;--------- Appearance
;; font settins and fontaine package setup
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell t)       ; visible-bell on
(setq sentence-end-double-space nil) ; sentence boundaries

;; Set basic fonts and display theme
(set-face-attribute 'default nil
		    :family "Iosevka"
		    :height 160
		    :weight 'semi-light)

(load-theme 'modus-operandi-tinted)

;;----------------------- display and fonts
;; fontaine - font management

(use-package fontaine
  :config
  (setq fontaine-presets
	'((regular :default-height 160)
	  (large :default-weight semilight
		 :default-height 190
		 :bold-weight extrabold)
	  (t :default-family "Iosevka"
	     :default-weight regular
	     :default-slant normal
	     :default-width normal
	     :default-height 120

	     :variable-pitch-family "ETBembo"
	     :variable-pitch-weight nil
	     :variable-pitch-slant nil
	     :variable-pitch-width nil
	     :variable-pitch-height 1.0))
	)
  ;; Set the last preset or fall back to desired style from `fontaine-presets'
  ;; (the `regular' in this case).
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)
  )

;; Mixed Pitch
(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Enable Vertico.

(use-package vertico
  :demand t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode)
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; which-key is installed after emacs30

(use-package which-key
  :demand t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1))

; setup functions to be called during package load
(defun tes/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

; setup function for font-setup in org
(defun tes/org-font-setup () (dolist (face '((org-level-1 . 1.5)
					     (org-level-2 . 1.3)
					     (org-level-3 . 1.2)
					     (org-level-4 . 1.1)
					     (org-level-5 . 1.1)
					     (org-level-6 . 1.1)
					     (org-level-7 . 1.1)
					     (org-level-8 . 1.1)))
			       (set-face-attribute (car face) nil :weight 'bold :height (cdr face))))

; a custom function for myself
; to auto add a property of the created time to new headings use
; heading of file to set :: #+PROPERTY: CREATED nil
(defun tes/org-set-created-property ()
  "Set the CREATED property of an Org heading when it is first created."
  (when (and (org-at-heading-p)
	     (let (flag)
	       (with-current-buffer (current-buffer)
		 (when (boundp 'tes-created-flag)
		   (setq flag (symbol-value 'tes-created-flag))))
	       flag)
	     (org-set-property "CREATED" (format-time-string "[%Y-%m-%d]")))))

;; org --- use-package call
(use-package org
  :init
  (setq org-directory "~/Dropbox/Org/")
  (setq org-agenda-files (list (expand-file-name "Agenda/" org-directory)))
  :hook ((org-mode . tes/org-mode-setup)
	 (org-insert-heading . tes/org-set-created-property))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))

  :custom
	   (org-special-ctrl-a/e t)
	   (org-use-speed-commands t)
	   (org-insert-heading-respect-content t)
	   (org-M-RET-may-split-line '((default . nil)))

	   ;; double spacing
	   (sentence-end-double-space nil)
	   
	   ;; logging
	   (org-log-done 'time)
	   (org-log-into-drawer t)

	   ;; agenda warning day
	   (org-deadline-warning-days 3)
	   
	   ;; Org styling, hide markup etc.
	   (org-pretty-entities t)
	   (org-ellipsis "…")

	   ;; jumping to locations C-c C-j org-goto function
	   (org-goto-interface 'outline-path-completion)
	   (org-outline-path-complete-in-steps nil)
	   
	   ;; Todo keywords  
	   (org-todo-keywords
	   '((sequence "TODO(t)" "WAIT(w!)"  "|" "CANCEL(c@)" "DONE(d!)")))

	   ;; Capture templates
	   (org-capture-templates
	   '(("t" "Personal todo" entry
	      (file+headline "Agenda/todo.org" "Inbox")
	      "* TODO %?\n %i\n" :prepend t)
	     ("i" "Idea" entry
	      (file+headline "Agenda/todo.org" "Idea")
	      "* IDEA %?\n %i\n" :prepend t)))

	   ;; consider adding to templates with one for meeting notes, that also goes to the journal file
	   (org-archive-location "archive.org::datetree//")

  (tes/org-font-setup))

;; magit --- configuration
(use-package magit)

;; ediff --- configuration
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; org-journal setup
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Dropbox/Org/Journal"
        org-journal-date-format "%A, %d %B %Y"))
