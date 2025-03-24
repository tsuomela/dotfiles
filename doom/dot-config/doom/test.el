(setq doom-font (font-spec :family "Iosevka" :size 16 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))
      ;; doom-symbol-font (font-spec :family "Symbols Nerd Font"))

(setq doom-theme 'doom-fairy-floss)

dk(setq display-line-numbers-type t)

(setq org-directory "~/Dropbox/Org/")

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
