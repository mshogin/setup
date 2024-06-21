(defun openai-answer (prompt)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " openai-api-key))))
         (data (json-encode `((prompt . ,prompt)))))
    (with-current-buffer
        (url-retrieve-synchronously "https://api.openai.com/v1/engines/davinci-codex/completions" nil nil nil nil
                                    (lambda (buffer _)
                                      (set-buffer buffer)
                                      (setq buffer-read-only nil)))
      (goto-char (point-min))
      (re-search-forward "^$")
      (forward-char)
      (let* ((response (json-read))
             (choices (cdr (assoc 'choices response)))
             (text (cdr (assoc 'text (car choices))))))
      text)))

(defun openai-answer-interactive ()
  (interactive)
  (let ((prompt (read-string "Введите ваш вопрос: ")))
    (message "%s" (openai-answer prompt))))


(message (openai-answer "Какой смысл жизни?"))

(setq python-interpreter "/usr/bin/python")
