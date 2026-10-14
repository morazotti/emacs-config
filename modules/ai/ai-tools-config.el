(use-package ragmacs
   :straight (:host github :repo "positron-solutions/ragmacs":branch "master")
   :after gptel)

(use-package llm-tool-collection
  :straight (:host github :repo "skissue/llm-tool-collection" :branch "main")
  :config
  (mapc (apply-partially #'apply #'gptel-make-tool)
        (llm-tool-collection-get-all)))

(provide 'ai-tools-config)
