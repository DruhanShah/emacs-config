(use-package evil
  :straight t
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
  :straight t
  :after evil
  :init
  (evil-collection-init))

(use-package evil-god-state
  :straight t
  :after evil
  :config
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (evil-define-key 'god global-map "escape" 'evil-god-state-bail))


(provide 'evil-bindings)
