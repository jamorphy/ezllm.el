# ezllm.el

WARNING: Only tested with Groq and OpenAI. Doesn't support Claude/Anthropic yet.

Basic setup for LLM streaming to buffer. I don't see myself adding any huge overhauls but use with caution. I'll add stuff as I need it. You should probably just use it as a reference for your own setup.

It's tailored to my preferences but I'm open to PRs.

## Usage
```
(load-file "~/.emacs.d/ezllm.el") ;; download the file somewhere

(setq ezllm-api-key "<API_KEY>")
(setq ezllm-endpoint "https://api.groq.com/openai/v1/chat/completions")
(setq ezllm-model "llama-3.1-8b-instant")
;; don't forget to reeval the above 3 lines when changing providers/model/etc

;; optional
(setq ezllm-system-prompt "You are a programming assistant that only outputs code.")
(setq ezllm-max-tokens 3500) ;; default 1024
(global-set-key (kbd "C-c m") 'ezllm-stream-region)
```
Now type in a query, select it, and invoke `llm-stream-region`
