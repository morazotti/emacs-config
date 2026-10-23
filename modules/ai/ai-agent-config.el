(use-package agent-shell
  :ensure t
  :config

  (setq agent-shell-omp-models (vector "anthropic/claude-opus-5-5"
                                       "anthropic/claude-sonnet-5-5"
                                       "anthropic/claude-haiku-5-5"
                                       "openai/gpt-5.6"
                                       "openai/gpt-5.5"
                                       "openai/gpt-5.4-mini"
                                       "google/gemini-3.8-flash"
                                       "google/gemini-3.5-flash-lite"))
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  
  (setq agent-shell-anthropic-claude-environment
        (let ((dwave-token (auth-source-pick-first-password
                            :host "dwave" :user "apikey")))
          (apply #'agent-shell-make-environment-variables
                 (append (when dwave-token
                           (list "DWAVE_API_TOKEN" dwave-token))
                         (list :inherit-env t)))))

  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))
  
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication
         :api-key (lambda () (auth-source-pass-get 'secret "google-api-key"))))
  (setq agent-shell-google-gemini-environment
        (agent-shell-make-environment-variables :inherit-env t))
  (setq agent-shell-google-gemini-acp-command
        '("gemini" "--experimental-acp" "--allowed-tools" "run_shell_command"))

  ;; Só estes modelos aparecem no seletor (C-c C-v). Ids exatos
  ;; "provider/modelo"; veja com `omp models'. O omp lê o filtro de um arquivo
  ;; de config (--config), gerado aqui em JSON, que também é YAML válido.
  (let ((overlay (make-temp-file "omp-agent-shell-" nil ".yml"
                                 (json-encode
                                  `((enabledModels . ,agent-shell-omp-models))))))
    (add-hook 'kill-emacs-hook
              `(lambda () (ignore-errors (delete-file ,overlay))))
    (setq agent-shell-omp-acp-command (list "omp" "acp" "--config" overlay)))

  (setq agent-shell-omp-environment
        (let ((openai-key (auth-source-pick-first-password
                           :host "api.openai.com"))
              (gemini-key (auth-source-pick-first-password
                           :host "generativelanguage.googleapis.com")))
          (apply #'agent-shell-make-environment-variables
                 (append (when openai-key
                           (list "OPENAI_API_KEY" openai-key))
                         (when gemini-key
                           (list "GEMINI_API_KEY" gemini-key))
                         (list :inherit-env t)))))
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c RET" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt)))


(provide 'ai-agent-config)
