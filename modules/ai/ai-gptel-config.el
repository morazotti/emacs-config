;; gptel
(use-package gptel
  :init
  ;; Avoid eager evaluation during byte-compilation (and before straight has
  ;; ensured the package is installed).
  (with-eval-after-load 'gptel
    (gptel-make-gemini "Gemini"
      :key (lambda ()
             (auth-source-pick-first-password
              :host "generativelanguage.googleapis.com"
              :user "apikey"))
      :stream t))
  :custom ((gptel-default-mode #'org-mode)
           (gptel-track-media t)
           (gptel-use-tools t)
           (gptel-model 'gemini-flash-latest)))

(with-eval-after-load 'gptel
  (require 'ai-gptel-tools-config)
  (require 'ai-gptel-presets-config))

(defun my/gptel-mode-auto ()
  "Ensure that this file opens with `gptel-mode' enabled."
  (save-excursion
    (modify-file-local-variable-prop-line
     'eval nil 'delete)
    (add-file-local-variable-prop-line
     'eval '(and (fboundp 'gptel-mode) (gptel-mode 1)))))

(add-hook 'gptel-save-state-hook #'my/gptel-mode-auto)

(use-package gptel-commit
  :straight (:host github :repo "lakkiy/gptel-commit")
  :after (gptel magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . gptel-commit)))

(use-package gptel-agent
  :config (gptel-agent-update))

;; (use-package gptel-org-tools
;;   :straight (:host codeberg :repo "bajsicki/gptel-got" :branch "main"))

(use-package gptel-quick
  :straight (:host github :repo "karthink/gptel-quick" :branch "master")
  :bind (:map embark-general-map ("?" . gptel-quick)))

(use-package mcp
  :ensure t
  :after gptel
  :config
  (let (( -mcp-filesystem-allowed-directories
	  (mapcar 'expand-file-name '("~/repos" "~/Projects"))))
    (setq mcp-hub-servers
	  `(("filesystem" . (:command "npx" 
				      :args ,(append '("-y" "@modelcontextprotocol/server-filesystem")
						     -mcp-filesystem-allowed-directories)))
	    ("fetch"      . (:command "uvx" 
				      :args ("mcp-server-fetch"))))))
  :config
  ;; Opcional: inicia os servidores automaticamente ao abrir o Emacs
  (add-hook 'after-init-hook #'mcp-hub-start-all-server))

(provide 'ai-gptel-config)
