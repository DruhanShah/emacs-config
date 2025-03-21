(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
       :rev :newest
       :branch "main")
  :ensure t
  :hook (prog-mode . copilot-mode)
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))

(use-package copilot-chat
  :vc (:url "https://github.com/chep/copilot-chat.el"
       :rev :newest
       :branch "main")
  :ensure t
  :after (request org markdown-mode))
