(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '("llama3.2"
	    "qwen2.5:7b"
	    "qwen2.5-coder:7b"
	    "minimax-m3:cloud"
	    "deepseek-r1:7b"))

(setq-default gptel-model "llama3.2")
(provide 'ai-ollama-config)
