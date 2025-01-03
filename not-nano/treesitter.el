(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(bash "https://github.com/tree-sitter/tree-sitter-bash")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(css "https://github.com/tree-sitter/tree-sitter-css")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(setq treesit-font-lock-level 4)


(provide 'treesitter)
