(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '("llama3.1"
	    "qwen2.5:7b"
	    "qwen2.5-coder:7b"
	    "deepseek-r1:7b"))

(setq-default gptel-model "llama3.1:8b")
(provide 'ai-ollama-config)
