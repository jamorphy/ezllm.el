;; -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

(setq groq-api-key "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")

(defun ezllm-parse-buffer (spec)
  "Parse the current buffer and generate JSON for the specified LLM spec."
  (interactive
   (list (intern (completing-read "Choose spec: " '("ezllm-openai" "ezllm-anthropic") nil t))))
  (let ((model nil)
        (system-prompt nil)
        (messages '())
        (current-role nil)
        (current-content ""))
    
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "MODEL:" line)
            (setq model (string-trim (substring line 6))))
           ((string-prefix-p "SYSTEM:" line)
            (setq system-prompt (string-trim (substring line 7))))
           ((string-prefix-p "USER:" line)
            (when current-role
              (push (list :role current-role :content (string-trim current-content)) messages)
              (setq current-content ""))
            (setq current-role "user")
            (setq current-content (concat current-content (string-trim (substring line 5)) "\n")))
           ((string-prefix-p "ASSISTANT:" line)
            (when current-role
              (push (list :role current-role :content (string-trim current-content)) messages)
              (setq current-content ""))
            (setq current-role "assistant")
            (setq current-content (concat current-content (string-trim (substring line 10)) "\n")))
           (t
            (setq current-content (concat current-content line "\n")))))
        (forward-line 1)))
    
    (when current-role
      (push (list :role current-role :content (string-trim current-content)) messages))
    
    (setq messages (nreverse messages))
    
    (let* ((spec-symbol (if (symbolp spec) spec (intern spec)))
           (spec-value (symbol-value spec-symbol))
           (request (funcall (plist-get spec-value :request-formatter)
                             messages system-prompt model 2048)))
      (json-encode request))))

(defun ezllm-log (message &rest args)
  "Log a debug MESSAGE with ARGS to the *EZLLM Debug* buffer."
  (with-current-buffer (get-buffer-create "*EZLLM Debug*")
    (goto-char (point-max))
    (insert (apply #'format (cons (format "[%s] %s\n" (current-time-string) message) args)))))

(defun ezllm-process-chunk (chunk marker)
  "Process a single chunk of streaming data and insert at MARKER."
  (ezllm-log "Processing chunk: %s" chunk)
  (when (string-prefix-p "data: " chunk)
    (let* ((json-string (substring chunk 6))
           (json-object (json-read-from-string json-string))
           (content (cdr (assoc 'content (cdr (assoc 'delta (elt (cdr (assoc 'choices json-object)) 0)))))))
      (ezllm-log "Extracted content: %s" content)
      (when content
        (with-current-buffer (marker-buffer marker)
          (save-excursion
            (goto-char marker)
            (insert content)
            (set-marker marker (point))))
        (redisplay t)))))

(defun ezllm-openai-request ()
  "Make a request to the API and stream the response to the current buffer."
  (interactive)
  (ezllm-log "Starting ezllm-openai-request")
  (let* ((api-key groq-api-key)  ; Replace with your actual API key
         (json-string (ezllm-parse-buffer 'ezllm-openai))
         (url "https://api.groq.com/openai/v1/chat/completions")  ; Verify this URL
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Accept" . "text/event-stream")
            ("Authorization" . ,(concat "Bearer " api-key))))  ; Verify if "Bearer " is needed
         (url-request-data json-string)
         (response-marker (progn
                            (goto-char (point-max))
                            (insert "\n\nASSISTANT: ")
                            (point-marker))))
    
    (ezllm-log "Request URL: %s" url)
    (ezllm-log "Request headers: %S" url-request-extra-headers)
    (ezllm-log "Request data: %s" url-request-data)
    
    (url-retrieve 
     url
     (lambda (status)
       (ezllm-log "Response status: %S" status)
       (if (plist-get status :error)
           (progn
             (ezllm-log "Error in response: %S" (plist-get status :error))
             (ezllm-log "Full response buffer content:")
             (ezllm-log "%s" (with-current-buffer (current-buffer)
                               (buffer-string))))
         (progn
           (goto-char (point-min))
           (re-search-forward "\n\n" nil t)  ; Find the end of headers
           (ezllm-log "Response headers: %s" (buffer-substring (point-min) (point)))
           (while (not (eobp))
             (let ((chunk (buffer-substring (point) (line-end-position))))
               (ezllm-log "Processing response chunk: %s" chunk)
               (condition-case err
                   (ezllm-process-chunk chunk response-marker)
                 (error (ezllm-log "Error processing chunk: %S" err))))
             (forward-line 1))
           (ezllm-log "Finished processing response")
           (with-current-buffer (marker-buffer response-marker)
             (goto-char (marker-position response-marker))
             (insert "\n\nUSER: ")
             (set-marker response-marker (point)))))
       (kill-buffer (current-buffer)))
     nil t)
    
    (message "Request sent. Streaming response to current buffer...")
    (ezllm-log "Request sent, awaiting response")))
