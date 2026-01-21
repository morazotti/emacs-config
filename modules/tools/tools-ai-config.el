;; gptel
(use-package gptel
  :config (gptel-agent-update)
  :custom ((gptel-default-mode #'org-mode)
	   (gptel-track-media t)
	   (gptel-use-tools t)
	   (gptel-model "gpt-5.2")))

(use-package gptel-commit
  :straight (:host github :repo "lakkiy/gptel-commit")
  :after (gptel magit)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . gptel-commit)))

(use-package gptel-agent)
(use-package gptel-org-tools
  :straight (:host codeberg :repo "bajsicki/gptel-got" :branch "main"))

(use-package ragmacs
   :ensure (:host github :repo "positron-solutions/ragmacs")
   :after gptel)

(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :config (mapcar (apply-partially #'apply #'gptel-make-tool)
		  (llm-tool-collection-get-all)))

(require 'tools-ai-preset-config)
(provide 'tools-ai-config)
