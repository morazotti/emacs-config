(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :stream t
  :models '((llama3.2
	     :description "Meta's efficient AI model, great for lightweight tasks and edge devices.")
	    (qwen2.5:7b
	     :description "An advanced LLM with 7B parameters, optimized for performance and flexibility.")
	    (qwen2.5-coder:7b
	     :description "Fast code generation and debugging in many languages.")
	    (deepseek-r1:7b
	     :description "Small but strong at reasoning, math, and code tasks.")
	    (minimax-m3:cloud
	     :description  "strong at reasoning, coding, and multilingual tasks.")))

(setq-default gptel-model "llama3.2")
(provide 'ai-ollama-config)
