;; ------------------------------------
;; Initialization file for Emacs
;; Very WIP
;;
;; Heavily borrowed from https://github.com/rougier/nano-emacs
;; and related packages
;; ------------------------------------


;; Initialize package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (eval-when-compile
  ;; (require 'use-package))

(setq straight-use-package-by-default t)

(use-package magit
  :straight t
  :bind ("C-x C-M-g" . magit))
(setq backup-directory-alist `(("." . "~/.backups")))

(add-to-list 'load-path "~/.config/emacs/nano")
(add-to-list 'load-path "~/.config/emacs/not-nano")
(add-to-list 'load-path ".")

;; Not Nano stuff
(require 'rainbow)
(require 'latex-stuff)
(require 'github-copilot)
;; (require 'completion)
(require 'ligatures)
(require 'org-stuff)
(require 'treesitter)
(require 'minibuffer-stuff)
(require 'esoteric-stuff)
(require 'evil-bindings)

;; Nano stuff
(require 'nano-theme)
(nano-mode)
(nano-light)
(require 'nano-modeline)
(nano-modeline nil nil t)
(require 'nano-splash)
(require 'nano-svg)
(require 'nano-agenda)
(require 'nano-read)
;; (require 'nano-writer)
(require 'nano-org)
;; (require 'nano-elfeed)
(require 'nano-term)
(require 'nano-kill)
