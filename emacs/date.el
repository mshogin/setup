;; ====================
;; insert date and time

(defvar iso-8601-date-time-format "%Y-%m-%dT%H:%M:%S%:z"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
;       (insert (let () (comment-start)))
       (insert (format-time-string iso-8601-date-time-format (current-time)))
       )



(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key (kbd "M-m i d") 'insert-current-date-time)


