(require 'url)
(require 'json)

;; Provider registry and current provider
(defvar ezllm-providers (make-hash-table :test 'equal))
(defvar ezllm-current-provider nil)

;; Provider specs
(defconst ezllm-openai
  '(:auth-header "Authorization"
    :auth-value-prefix "Bearer "
    :request-formatter
    (lambda (prompt system-prompt model max-tokens)
      `(("model" . ,model)
        ("messages" . [((role . "system")
                        (content . ,system-prompt))
                       ((role . "user")
                        (content . ,prompt))])
        ("stream" . t)
        ("max_tokens" . ,max-tokens)))
    :response-parser
    (lambda (data)
      (condition-case nil
          (let* ((json-object-type 'plist)
                 (json-key-type 'keyword)
                 (parsed (json-read-from-string data))
                 (choices (plist-get parsed :choices))
                 (delta (plist-get (aref choices 0) :delta))
                 (content (plist-get delta :content)))
            content)
        (error nil)))))

(defconst ezllm-anthropic
  '(:auth-header "x-api-key"
    :auth-value-prefix ""
    :extra-headers (("anthropic-version" . "2023-06-01"))
    :request-formatter
    (lambda (prompt system-prompt model max-tokens)
      `(("model" . ,model)
        ("messages" . [((role . "user")
                        (content . ,prompt))])
        ("system" . ,system-prompt)
        ("stream" . t)
        ("max_tokens" . ,max-tokens)))
    :response-parser
    (lambda (data)
      (condition-case nil
          (let* ((json-object-type 'plist)
                 (json-key-type 'keyword)
                 (parsed (json-read-from-string data))
                 (delta (plist-get parsed :delta))
                 (text (plist-get delta :text)))
            text)
        (error nil)))))

;; Configuration function
(defun ezllm-configure-provider (&rest args)
  "Configure a provider with the given ARGS."
  (let* ((name (or (plist-get args :name)
                   (error "Provider name is required")))
         (spec (plist-get args :spec))
         (endpoint (plist-get args :endpoint))
         (model (plist-get args :model))
         (max-tokens (plist-get args :max-tokens))
         (api-key (plist-get args :api-key))
         (system-prompt (plist-get args :system-prompt)))
    (puthash name
             (list :spec spec
                   :endpoint endpoint
                   :model model
                   :max-tokens max-tokens
                   :api-key api-key
                   :system-prompt system-prompt)
             ezllm-providers)
    (message "Debug: Provider %s configured. Current providers: %s" 
             name (hash-table-keys ezllm-providers))
    (unless ezllm-current-provider
      (setq ezllm-current-provider name))
    (message "Provider %s configured successfully" name)))

;; Set current provider
(defun ezllm-set-provider (provider-name)
  "Set the current provider to PROVIDER-NAME."
  (let ((provider-symbol (if (symbolp provider-name) provider-name (intern provider-name))))
    (message "Debug: Trying to set provider '%s'" provider-symbol)
    (message "Debug: Current providers: %s" (hash-table-keys ezllm-providers))
    (if (gethash provider-symbol ezllm-providers)
        (progn
          (setq ezllm-current-provider provider-symbol)
          (message "Current provider set to %s" provider-symbol))
      (message "Provider %s not configured. Please configure it first using ezllm-configure-provider." provider-symbol))))

;; Handle API response
(defun ezllm-handle-response (status)
  (if (plist-get status :error)
      (message "Request failed: %S" (plist-get status :error))
    (let* ((provider-config (gethash ezllm-current-provider ezllm-providers))
           (spec (plist-get provider-config :spec))
           (response-parser (plist-get spec :response-parser)))
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (forward-char)
      (delete-region (point-min) (point))
      (while (not (eobp))
        (let ((line (buffer-substring (point) (line-end-position))))
          (when (string-prefix-p "data: " line)
            (let ((data (substring line 6)))
              (unless (string= data "[DONE]")
                (condition-case nil
                    (let ((content (funcall response-parser data)))
                      (when content
                        (with-current-buffer ezllm-output-buffer
                          (goto-char ezllm-output-marker)
                          (insert content)
                          (setq ezllm-output-marker (point-marker))
                          (redisplay t))))
                  (error nil)))))
          (forward-line))))))

;; Make streaming request
(defun ezllm-stream-request (prompt)
  "Make a streaming request with the given PROMPT."
  (let* ((provider-config (gethash ezllm-current-provider ezllm-providers))
         (spec (plist-get provider-config :spec))
         (endpoint (plist-get provider-config :endpoint))
         (model (plist-get provider-config :model))
         (max-tokens (plist-get provider-config :max-tokens))
         (api-key (plist-get provider-config :api-key))
         (system-prompt (plist-get provider-config :system-prompt))
         (auth-header (plist-get spec :auth-header))
         (auth-value-prefix (plist-get spec :auth-value-prefix))
         (extra-headers (plist-get spec :extra-headers))
         (request-formatter (plist-get spec :request-formatter))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Accept" . "text/event-stream")
            (,auth-header . ,(concat auth-value-prefix api-key))
            ,@extra-headers))
         (url-request-data
          (json-encode (funcall request-formatter prompt system-prompt model max-tokens))))
    (url-retrieve endpoint #'ezllm-handle-response nil t)))

;; User-facing function to stream from selected region
(defun ezllm-stream-region (start end)
  "Stream LLM response for the region between START and END."
  (interactive "r")
  (let ((prompt (buffer-substring-no-properties start end)))
    (setq ezllm-output-buffer (current-buffer))
    (goto-char end)
    (insert "\n\n")
    (setq ezllm-output-marker (point-marker))
    (ezllm-stream-request prompt)))

(provide 'ezllm)
