(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '((llama3.1:8b
	     :description "Meta's efficient AI model, great for lightweight tasks and edge devices.")
	    (qwen3.5:9b-Q4_K_M
	     :description "An advanced LLM with 9B parameters, optimized for performance and flexibility.")
	    (qwen3.5:9b
	     :description "An advanced LLM with 9B parameters, optimized for performance and flexibility.")
	    (gemma2:9b
	     :description "Google's model with incredible cohesion")
	    (minimax-m3:cloud
	     :description  "strong at reasoning, coding, and multilingual tasks.")))

(setq-default gptel-model "llama3.2")
(provide 'ai-ollama-config)
