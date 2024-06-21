(defun eshell-toggle ()
  "Toggle between eshell buffers.
If you are in a shell buffer, switch the window configuration
back to your code buffers.  Otherwise, create at least one shell
buffer if it doesn't exist already, and switch to it.  On every
toggle, the current window configuration is saved in a register."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (jump-to-register ?W)
    ;; Save current window config and jump to shell
    (window-configuration-to-register ?W)
    (condition-case nil
        (jump-to-register ?Z)
      (error
       (eshell)
       (when (= (length (window-list)) 2)
         (other-window 1)
         (eshell 1)
         (other-window 1))))
    (window-configuration-to-register ?Z)))
(global-set-key (kbd "C-q") 'eshell-toggle)

(defalias 'ff 'find-file)
;; (defalias 'll 'ls -la $*')

(require 'eshell)

(defun eshell-load-bash-aliases ()
  "Reads bash aliases from Bash and inserts
    them into the list of eshell aliases."
  (interactive)
  (progn
    (message "Parsing aliases")
    (shell-command "alias" "bash-aliases" "bash-errors")
    (switch-to-buffer "bash-aliases")
    (replace-string "alias " "")
    (goto-char 1)
    (replace-string "='" " ")
    (goto-char 1)
    (replace-string "'\n" "\n")
    (goto-char 1)
    (let ((alias-name) (command-string) (alias-list))
      (while (not (eobp))
        (while (not (char-equal (char-after) 32))
          (forward-char 1))
        (setq alias-name
              (buffer-substring-no-properties (line-beginning-position) (point)))
        (forward-char 1)
        (setq command-string
              (buffer-substring-no-properties (point) (line-end-position)))
        (setq alias-list (cons (list alias-name command-string) alias-list))
        (forward-line 1))
      (setq eshell-command-aliases-list alias-list))
    (if (get-buffer "bash-aliases")(kill-buffer "bash-aliases"))
    (if (get-buffer "bash-errors")(kill-buffer "bash-errors"))))

(add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)

(setq eshell-buffer-maximum-lines 2000000)
