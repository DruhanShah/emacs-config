(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :ensure t
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))


(provide 'github-copilot)
