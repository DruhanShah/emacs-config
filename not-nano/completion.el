(use-package corfu
  :straight t
  :config
  (setq corfu-auto t)
  (add-hook 'after-init-hook 'global-corfu-mode))

(provide 'completion)
