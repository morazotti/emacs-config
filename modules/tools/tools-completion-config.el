;; corfu para auto-complete
(use-package corfu
  :init (global-corfu-mode))

;; eglot para LSP
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (javascript-mode . eglot-ensure)
  (julia-mode . eglot-ensure)
  (java-mode . eglot-ensure))

(add-to-list 'exec-path "/home/nicolas/.local/bin")
(use-package eglot-booster
  :after eglot
  :straight (:host github :repo "jdtsmith/eglot-booster" :branch "main")
  :config (eglot-booster-mode))

;; mason manages lsp servers
(use-package mason
  :config (mason-setup))

;; (use-package json-rpc)
(provide 'tools-completion-config)
