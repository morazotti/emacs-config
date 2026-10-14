(use-package agent-shell
  :ensure t
  :config
  ;; Usa o login da assinatura (é o padrão, mas deixe explícito)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  ;; Herda PATH e HOME do Emacs, para achar o node/claude
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))
  ;; Faz o Claude ser o agente padrão do M-x agent-shell
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config))
  ;; Agente secundário: Gemini CLI, iniciado sob demanda com
  ;; M-x agent-shell-google-start-gemini (o Claude continua o padrão)
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  (setq agent-shell-google-gemini-environment
        (agent-shell-make-environment-variables :inherit-env t))
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-c C-c" . shell-maker-submit)
              ("C-c C-k" . agent-shell-interrupt)))

(provide 'ai-agent-config)
