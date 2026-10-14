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
  (require 'ai-gptel-presets-config)
  (require 'ai-gptel-ollama-config))

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

(provide 'ai-gptel-config)
