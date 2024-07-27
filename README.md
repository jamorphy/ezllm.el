# ezllm.el

WARNING: not stable, use as reference for now

Basic setup for LLM streaming to buffer. I don't see myself adding any huge overhauls but use with caution.

It's tailored to my preferences but I'm open to PRs. I'll put my full personal config in another branch.

## Example config
```
(require 'ezllm)

(ezllm-configure-provider :name 'llama-3.1-8b      ;; can be anything you want
                          :spec ezllm-openai
                          :endpoint "https://api.groq.com/openai/v1/chat/completions"
                          :model "llama-3.1-8b-instant"
                          :max-tokens 3000
                          :api-key "API_KEY_HERE"
                          :system-prompt "You are a code generator. Only output code.")

(ezllm-configure-provider :name 'sonnet-3.5
                          :spec ezllm-anthropic
                          :endpoint "https://api.anthropic.com/v1/messages"
                          :model "claude-3-5-sonnet-20240620"
                          :max-tokens 1024
                          :api-key "API_KEY_HERE"
                          :system-prompt "You are a helpful assistant.")

(ezllm-configure-provider :name 'gpt-4-turbo
                          :spec ezllm-openai
                          :endpoint "https://api.openai.com/v1/chat/completions"
                          :model "gpt-4-turbo"
                          :max-tokens 2048
                          :api-key "API_KEY_HERE"
                          :system-prompt "Respond only in pig latin.")

(global-set-key (kbd "C-c m") 'ezllm-send)

;; default provider
(ezllm-set-provider 'sonnet-3.5)
```

## Usage
Send the current line, or selected region with `ezllm-send`.

`(ezllm-send "Write hello world in C++.")` works too.

To change providers, use `(ezllm-set-provider 'mistral-large-2)` or `M-x ezllm-set-provider`.
