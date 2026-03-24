(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '("llama3.1:8b"
	    "phi3.5:latest"
	    "deepseek-coder-v2:lite"))

(defun my/ollama-active-p ()
  "Return t if the Ollama service is active in systemd."
  (let ((status (string-trim (shell-command-to-string "systemctl is-active ollama"))))
    (string= status "active")))

(defun ollama-status-toggle ()
  "Toggle the Ollama service based on the predicate my/ollama-active-p."
  (interactive)
  (if (my/ollama-active-p)
      (progn
        (shell-command "sudo systemctl stop ollama")
        (message "Started ollama."))
    (progn
      (shell-command "sudo systemctl start ollama")
      (message "Stopped ollama."))))

;; (advice-add 'gptel-send :before
;;             (lambda (&rest _)
;;               (unless (my/ollama-active-p)
;;                 (when (y-or-n-p "Ollama is off. Do you want to start it now?")
;;                   (ollama-status-toggle)
;;                   ;; Give a small delay for the socket to open
;;                   (sleep-for 1)))))

(setq-default gptel-model "llama3.1:8b")
(provide 'ai-ollama-config)
