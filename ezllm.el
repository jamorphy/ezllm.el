(require 'url)
(require 'json)

(setq llm-api-key "")
(setq llm-endpoint "")
(setq llm-model "")
(setq llm-system-prompt "You are a helpful assistant.")
(setq llm-max-tokens 1024)

(defun llm-stream-request (prompt)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " llm-api-key))))
         (request-data `(("messages" . [((role . "system")
                                         (content . ,llm-system-prompt))
                                        ((role . "user")
                                         (content . ,prompt))])
                         ("model" . ,llm-model)
                         ("max_tokens" . ,llm-max-tokens)
                         ("stream" . t)))
         (url-request-data (json-encode request-data))
         (response-buffer (generate-new-buffer " *llm-response*")))
    (url-retrieve llm-endpoint 
                  #'llm-stream-callback 
                  (list response-buffer (current-buffer))
                  t)))

(defun llm-stream-callback (status response-buffer target-buffer)
  "Handle the response from the API."
  (unless (plist-get status :error)
    (let ((data (buffer-string)))
      (with-current-buffer response-buffer
        (erase-buffer)
        (insert data)
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (delete-region (point-min) (point))
        (llm-stream-process-chunks target-buffer)))))

(defun llm-stream-process-chunks (target-buffer)
  "Process the chunks of data in the response buffer."
  (while (not (eobp))
    (let ((chunk (buffer-substring (point) (line-end-position))))
      (when (string-prefix-p "data: " chunk)
        (let ((json-string (substring chunk 6)))
          (unless (string= json-string "[DONE]")
            (ignore-errors
              (let* ((json-object (json-read-from-string json-string))
                     (choices (assoc-default 'choices json-object))
                     (delta (and choices (assoc-default 'delta (aref choices 0))))
                     (content (and delta (assoc-default 'content delta))))
                (when content
                  (with-current-buffer target-buffer
                    (insert content)
                    (redisplay t))))))))
    (forward-line))))

(defun llm-stream-region (start end)
  "Use the highlighted region as a prompt for the LLM and process the streaming response."
  (interactive "r")
  (cond
   ((or (null llm-api-key) (string-empty-p llm-api-key))
    (message "Error: API key is not set. Please set llm-api-key."))
   ((or (null llm-endpoint) (string-empty-p llm-endpoint))
    (message "Error: Endpoint URL is not set. Please set llm-endpoint."))
   ((or (null llm-model) (string-empty-p llm-model))
    (message "Error: Model is not set. Please set llm-model."))
   ((not (use-region-p))
    (message "No region selected. Please highlight text to use as a prompt."))
   (t
    (let ((prompt (buffer-substring-no-properties start end)))
      (deactivate-mark)
      (goto-char end)
      (insert "\n\n")
      (llm-stream-request prompt)))))

(provide 'ezllm)
