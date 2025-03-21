;; ---------------------------------------------
;;              EMACS INIT FILE
;; 
;; This is where all of my configuration for
;; GNU Emacs resides. Please read README.org for
;; more detailed information about the config.
;; ---------------------------------------------

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca :repo "https://github.com/progfolio/elpaca.git"
           :ref nil :depth 1 :inherit ignore
           :files (:defaults "elpaca-test.el" (:exclude "extensions"))
           :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone"
                             ,@(when-let* ((depth (plist-get order :depth)))
                                 (list
				  (format "--depth=%d" depth)
				  "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process
                           emacs nil buffer nil "-Q" "-L" "." "--batch"
                           "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(set-frame-parameter (selected-frame) 'background-mode 'light)
(add-to-list 'load-path "~/.config/emacs/nano")

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (setq evil-default-state 'normal)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

(use-package evil-god-state
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (evil-define-key 'god global-map "escape" 'evil-god-state-bail))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t)

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
                "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>"
                "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::"
		"==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>"
		"+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))


(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :ensure t
  :after nerd-icons
  :hook (nerd-icons-completion-mode))

(require 'dired)
(setq dired-listing-switches
      "-l --almost-all --human-readable --group-directories-first")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install)
  :hook (pdf-view-mode . (lambda () (pdf-view-themed-minor-mode 1))))

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t)
  (global-corfu-mode))

(use-package eat
  :ensure t)

(use-package org
  :ensure (:type built-in)
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . mixed-pitch-mode)
  :hook (org-mode . org-display-inline-images)
  :config
  (setq org-ellipsis "..."
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-use-sub-superscripts '{}
        org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
        org-latex-src-block-backend 'listings
        org-todo-keywords
        '((sequence "TODO" "DOING" "|" "DONE")
	  (sequence "LATER" "|" "NOPE"))
	org-priority-highest 0
	org-priority-lowest 9
	org-priority-default 5
        org-agenda-files
        '("~/Notes/Productivity/gcal.org"
          "~/Notes/Productivity/tasks.org"
          "~/Notes/Productivity/appointments.org")
	org-hidden-keywords '(title author date)
        org-capture-templates
        '(("t" "Todo" entry
	   (file+headline "~/Notes/Productivity/refile.org" "Tasks to refile")
           "* TODO %?\n  %i\n  %a")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t))))

(use-package ob-ipython
    :ensure t
    :after org
    :config
    (add-to-list 'org-babel-load-languages '(ipython . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

  (use-package jupyter
    :ensure t
    :after org
    :config
    (add-to-list 'org-babel-load-languages '(jupyter . t))
    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package ox-ipynb
  :ensure (ox-ipynb :host github :repo "jkitchin/ox-ipynb")
  :after org)

(use-package org-auto-tangle
  :ensure t
  :after org
  :hook (org-mode . org-auto-tangle-mode))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "" "✸" "")))

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package mixed-pitch
  :ensure t
  :after org
  :hook (org-mode . mixed-pitch-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(setq treesit-font-lock-level 4)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :commands (vertico-mode)
  :init
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  (defvar vertico-preselect)
  :config (setq vertico-preselect 'first))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function 'consult-completion-in-region))

(use-package dyalog-mode
  :ensure t
  :config
  (defun org-babel-execute:dyalog (body params)
    (org-babel-execute:jupyter-apl body params))

  (defun org-babel-dyalog-initiate-session (&optional arg1 arg2)
    (org-babel-jupyter-apl-initiate-session &optional arg1 arg2))

  (setq org-babel-default-header-args:jupyter-apl '((:kernel . "dyalog_apl")
                                                    (:session . "*new*")
                                                    (:exports . "both")
                                                    (:eval . "never-export"))

        org-babel-default-header-args:dyalog '((:kernel . "dyalog_apl")
                                               (:session . "*new*")
                                               (:exports . "both")
                                               (:eval . "never-export"))))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (progn
              (require 'nano-theme)
              (nano-mode)
              (nano-light)

	      (require 'nano-modeline)
	      (nano-modeline nil nil t)

	      (require 'nano-splash)
	      (require 'nano-calendar)
	      (require 'nano-agenda)

	      (require 'nano-vertico)
	      (nano-vertico-mode)
	      (require 'nano-read)

	      (require 'nano-writer)
	      (require 'nano-org)

	      ;; (require 'nano-elfeed)
	      (require 'nano-term)
	      (require 'nano-kill)
	      (require 'nano-block)

	      (require 'nano-box))))
