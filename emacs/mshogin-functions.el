(defun generate-strong-password (length)
  "Generate a strong password of LENGTH."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=[]{}|;:',.<>?/")
        (password ""))
    (dotimes (_ length password)
      (setq password
            (concat password
                    (string (aref chars (random (length chars)))))))))

(generate-strong-password 32)
