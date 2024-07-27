(require 'url)
(require 'json)
(require 'cl-lib)

(cl-defstruct (ezllm-provider (:constructor ezllm-make-provider))
  name
  spec
  endpoint
  model
  max-tokens
  api-key
  system-prompt)

(defvar ezllm-provider-names '()
  "List of configured provider names.")

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
  (let* ((name (plist-get args :name))
         (provider (apply #'ezllm-make-provider args)))
    (if name
        (progn
          (puthash name provider ezllm-providers)
          (add-to-list 'ezllm-provider-names name)
          (unless ezllm-current-provider
            (setq ezllm-current-provider name)))
      (error "Provider name is required"))))

;; Handle API response
(defun ezllm-handle-response (status)
  (if (plist-get status :error)
      (message "Request failed: %S" (plist-get status :error))
    (let* ((provider-config (gethash ezllm-current-provider ezllm-providers))
           (spec (ezllm-provider-spec provider-config))
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
                          (let ((decoded-content (decode-coding-string content 'utf-8)))
                            (goto-char ezllm-output-marker)
                            (insert decoded-content)
                            (setq ezllm-output-marker (point-marker))
                            (redisplay t)))))
                  (error nil)))))
          (forward-line))))))

(defun ezllm-stream-request (prompt provider)
  "Make a streaming request with the given PROMPT using the specified PROVIDER."
  (let* ((provider-config (gethash provider ezllm-providers))
         (spec (ezllm-provider-spec provider-config))
         (endpoint (ezllm-provider-endpoint provider-config))
         (model (ezllm-provider-model provider-config))
         (max-tokens (ezllm-provider-max-tokens provider-config))
         (api-key (ezllm-provider-api-key provider-config))
         (system-prompt (ezllm-provider-system-prompt provider-config))
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
    (setq ezllm-current-provider provider) ; Set the current provider for response handling
    (url-retrieve endpoint #'ezllm-handle-response nil t)))

(defun ezllm-send (&optional input-text provider)
  "Stream LLM response for the given INPUT-TEXT, selected region, or current line.
If INPUT-TEXT is provided, use that as the prompt.
If INPUT-TEXT is nil, use the selected region or current line as the prompt.
If PROVIDER is specified, use that provider; otherwise, use the current provider."
  (interactive)
  (let* ((prompt (cond
                  (input-text input-text)
                  ((use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end)))
                  (t (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
         (current-provider (or provider ezllm-current-provider)))
    (if (string-empty-p prompt)
        (message "No text provided or selected, and current line is empty. Nothing to send.")
      (set-buffer-file-coding-system 'utf-8)
      (setq ezllm-output-buffer (current-buffer))
      (if (called-interactively-p 'any)
          (progn
            (if (use-region-p)
                (goto-char (region-end))
              (end-of-line))
            (insert "\n\n"))
        (goto-char (point-max))
        (unless (looking-back "\n\n" (- (point) 2))
          (insert "\n\n")))
      (setq ezllm-output-marker (point-marker))
      (ezllm-stream-request prompt current-provider))))

(provide 'ezllm)
