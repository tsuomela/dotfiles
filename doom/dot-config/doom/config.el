;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))
      ;; doom-symbol-font (font-spec :family "Symbols Nerd Font"))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-fairy-floss)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
dk(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;; Some keybindings.
;;
;;
(map! "C-x C-r" #'consult-recent-file)

(use-package! doom-modeline
  :config
  (setq doom-modeline-enable-word-count t))  ;; Enables word count in modeline

(after! org
   (setq org-todo-keywords
       '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "MTGN(m)" "DONE(d)" "KILL(k)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
   (setq org-capture-templates
         '(("t" "Personal todo" entry
            (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n %i\n %a" :prepend t)
           ("j" "Journal" entry
            (file+olp+datetree +org-capture-journal-file)
            "* %U %?\n %i\n %a" :prepend t)
         ))
   (setq org-archive-location "archive.org::datetree//")
   (map! "C-c l" #'org-store-link
         "C-c a" #'org-agenda
         "C-c c" #'org-capture)
   (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
   (dolist (face '((org-level-1 . 1.5)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face))))
  ;; (use-package! org-modern
  ;;   :config
  ;;   (setq
  ;;    org-auto-align-tags t
  ;;    org-tags-column t
  ;;    org-fold-catch-invisible-edits 'show-and-error
  ;;    org-special-ctrl-a/e t
  ;;    org-insert-heading-respect-content t))
  ;; (use-package! org-superstar
  ;;   :config
  ;;   (org-superstar-configure-like-org-bullets)))
  ;;(setq org-superstar-leading-bullet " ")
  ;;(setq org-superstar-special-todo-items t))

;; trying evil-dvorak
;;(use-package! evil-dvorak
;;  :ensure t
;;  :config (global-evil-dvorak-mode 1))
