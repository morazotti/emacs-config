;; better parenthesis
(use-package smartparens
  :hook ((org-mode latex-mode C-mode julia-mode python-mode) .
	 smartparens-mode))

(use-package paredit
  :hook ((lisp-mode lisp-interaction-mode clojure-mode emacs-lisp-mode) .
	 paredit-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode LaTeX-mode latex-mode org-mode) . rainbow-delimiters-mode))

;; mise
;; helps with binaries installed with mise
(use-package mise
  :config (mise-mode))

;; snippets
;; actually, I'm checking tempel, and it looks good.
;; (add-to-list 'load-path
;;              (file-name-concat user-emacs-directory "plugins" "yasnippet"))
(use-package yasnippet
  :init (yas-global-mode))
(use-package yasnippet-snippets)

(provide 'langs-utils-config)
