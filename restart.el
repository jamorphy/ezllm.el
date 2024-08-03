;; -*- lexical-binding: t; -*-

(require 'url)
(require 'json)

(defun ezllm-parse-buffer (spec)
  "Parse the current buffer and generate JSON for the specified LLM spec."
  (interactive
   (list (intern (completing-read "Choose spec: " '("ezllm-openai" "ezllm-anthropic") nil t))))
  (let ((model nil)
        (system-prompt nil)
        (endpoint nil)
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
           ((string-prefix-p "ENDPOINT:" line)
            (setq endpoint (string-trim (substring line 9))))
           ((string-prefix-p "USER:" line)
            (when current-role
              (push (list :role current-role :content (string-trim current-content)) messages)
              (setq current-content ""))
            (setq current-role "user")
            (setq current-content (string-trim (substring line 5))))
           ((string-prefix-p "ASSISTANT:" line)
            (when current-role
              (push (list :role current-role :content (string-trim current-content)) messages)
              (setq current-content ""))
            (setq current-role "assistant")
            (setq current-content (string-trim (substring line 10))))
           (t
            (setq current-content (concat current-content "\n" (string-trim line))))))
        (forward-line 1)))
    
    (when current-role
      (push (list :role current-role :content (string-trim current-content)) messages))
    
    (setq messages (nreverse messages))
    
    (when system-prompt
      (push (list :role "system" :content system-prompt) messages))
    
    (let* ((spec-symbol (if (symbolp spec) spec (intern spec)))
           (spec-value (symbol-value spec-symbol))
           (request (list :model model
                          :messages (vconcat (mapcar (lambda (msg)
                                                       (list (cons 'role (plist-get msg :role))
                                                             (cons 'content (plist-get msg :content))))
                                                     messages))
                          :stream t
                          :max_tokens 2048)))
      (list :json (json-encode request) :endpoint endpoint))))

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

(defun ezllm-chat ()
  "Parse the buffer, prepare the request, and initiate the chat with the LLM."
  (interactive)
  (let* ((parsed-data (ezllm-parse-buffer 'ezllm-openai))
         (response-marker (progn
                            (goto-char (point-max))
                            (insert "\n\nASSISTANT: ")
                            (point-marker))))
    (ezllm-openai-request 
     parsed-data
     (lambda (chunk)
       (if (eq chunk 'eof)
           (progn
             (ezllm-log "End of response")
             (with-current-buffer (marker-buffer response-marker)
               (goto-char (marker-position response-marker))
               (insert "\n\nUSER: ")
               (set-marker response-marker (point))))
         (condition-case err
             (ezllm-process-chunk chunk response-marker)
           (error (ezllm-log "Error processing chunk: %S" err))))))))

(defun ezllm-openai-request (request-data callback)
  "Make a request to the API and call CALLBACK with each chunk of the response."
  (ezllm-log "Starting ezllm-openai-request")
  (let* ((json-string (plist-get request-data :json))
         (url (or (plist-get request-data :endpoint) "https://api.groq.com/openai/v1/chat/completions"))
         (api-key (or (plist-get request-data :api-key) mapi-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Accept" . "text/event-stream")
            ("Authorization" . ,(concat "Bearer " api-key))))
         (url-request-data json-string))
    
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
           (re-search-forward "\n\n" nil t)
           (ezllm-log "Response headers: %s" (buffer-substring (point-min) (point)))
           (while (not (eobp))
             (let ((chunk (buffer-substring (point) (line-end-position))))
               (ezllm-log "Processing response chunk: %s" chunk)
               (funcall callback chunk))
             (forward-line 1))
           (ezllm-log "Finished processing response")
           (funcall callback 'eof)))  ; Signal end of response
       (kill-buffer (current-buffer)))
     nil t)
    
    ;;(message "Request sent. Streaming response...")
    (ezllm-log "Request sent, awaiting response")))

(defun ezllm-quick (model endpoint system tokens &optional api-key)
  "Send a quick prompt to the specified MODEL and ENDPOINT with SYSTEM message and max TOKENS.
Use the selected region as the prompt, or the current line if no region is active.
If API-KEY is provided, it will be used instead of the default."
  (interactive)
  
  (let* ((prompt (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (json-string (json-encode
                       `(("model" . ,model)
                         ("messages" . [(("role" . "system")
                                         ("content" . ,system))
                                        (("role" . "user")
                                         ("content" . ,prompt))])
                         ("max_tokens" . ,tokens)
                         ("stream" . t))))
         (request-data `(:json ,json-string :endpoint ,endpoint :api-key ,api-key))
         (response-marker (progn
                            (if (use-region-p)
                                (goto-char (region-end))
                              (end-of-line))
                            (insert "\n\n")
                            (point-marker))))
    
    (ezllm-openai-request 
     request-data
     (lambda (chunk)
       (if (eq chunk 'eof)
           (ezllm-log "End of quick response")
         (condition-case err
             (ezllm-process-chunk chunk response-marker)
           (error (ezllm-log "Error processing chunk: %S" err))))))))

(defun ezllm-new-chat ()
  "Create a new buffer for an LLM chat with predefined content."
  (interactive)
  (let ((buffer (generate-new-buffer "*LLM Chat*")))
    (with-current-buffer buffer
      (insert "MODEL:\nENDPOINT:\nSYSTEM:\n\nUSER:\n")
      (goto-char (point-min)))
    (switch-to-buffer buffer)))
