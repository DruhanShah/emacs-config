(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package vertico
  :straight t
  :commands (vertico-mode)
  :init
  (vertico-mode 1)
  (defvar vertico-preselect)
  :config (setq vertico-preselect 'first))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package orderless
  :straight t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :straight t
  :config
  (setq completion-in-region-function 'consult-completion-in-region))


(provide 'minibuffer-stuff)
