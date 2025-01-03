(require 'org-margin)


(use-package org
  :straight (:type built-in)
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (setq org-ellipsis "..."
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-use-sub-superscripts '{}
        org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
        org-latex-src-block-backend 'listings
        org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE")
	  (sequence "LATER" "|" "NOPE"))
        org-agenda-files
        '("~/Notes/gcal.org"
          "~/Notes/tasks.org"
          "~/Notes/appointments.org")
	org-hidden-keywords '(title author date)
        org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Notes/refile.org" "Tasks to refile")
           "* TODO %?\n  %i\n  %a")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))


(use-package ob-ipython
  :straight t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(ipython . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package jupyter
  :straight t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(jupyter . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "" "✸" "")))

(use-package org-autolist
  :straight t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package markdown-mode
  :straight t)


(provide 'org-stuff)
