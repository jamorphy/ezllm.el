(require 'url)
(require 'json)

(setq ezllm-api-key "")
(setq ezllm-endpoint "")
(setq ezllm-model "")
(setq ezllm-system-prompt "You are a helpful assistant.")
(setq ezllm-max-tokens 1024)

(defun ezllm-stream-request (prompt)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " ezllm-api-key))))
         (request-data `(("messages" . [((role . "system")
                                         (content . ,ezllm-system-prompt))
                                        ((role . "user")
                                         (content . ,prompt))])
                         ("model" . ,ezllm-model)
                         ("max_tokens" . ,ezllm-max-tokens)
                         ("stream" . t)))
         (url-request-data (json-encode request-data))
         (response-buffer (generate-new-buffer " *llm-response*")))
    (url-retrieve ezllm-endpoint
                  #'ezllm-stream-callback
                  (list response-buffer (current-buffer))
                  t)))

(defun ezllm-stream-callback (status response-buffer target-buffer)
  "Handle the response from the API."
  (unless (plist-get status :error)
    (let ((data (buffer-string)))
      (with-current-buffer response-buffer
        (erase-buffer)
        (insert data)
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (delete-region (point-min) (point))
        (ezllm-stream-process-chunks target-buffer)))))

(defun ezllm-stream-process-chunks (target-buffer)
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

(defun ezllm-stream-region (start end)
  "Use the highlighted region as a prompt for the LLM and process the streaming response."
  (interactive "r")
  (cond
   ((or (null ezllm-api-key) (string-empty-p ezllm-api-key))
    (message "Error: API key is not set. Please set ezllm-api-key."))
   ((or (null ezllm-endpoint) (string-empty-p ezllm-endpoint))
    (message "Error: Endpoint URL is not set. Please set ezllm-endpoint."))
   ((or (null ezllm-model) (string-empty-p ezllm-model))
    (message "Error: Model is not set. Please set ezllm-model."))
   ((not (use-region-p))
    (message "No region selected. Please highlight text to use as a prompt."))
   (t
    (let ((prompt (buffer-substring-no-properties start end)))
      (deactivate-mark)
      (goto-char end)
      (insert "\n\n")
      (ezllm-stream-request prompt)))))

(provide 'ezllm)
