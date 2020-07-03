(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; ;; Optional - provides fancier overlays.
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; (setq gofmt-command "goimports")
;; (add-hook 'before-save-hook 'gofmt-before-save)

(defun spacemacs/go-run-test-current-function ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (spacemacs/go-run-tests
         (cond (go-use-testify-for-testing (concat " -testify.m='" (match-string-no-properties 3) "'"))
               (go-use-gocheck-for-testing (concat "-check.f='" (match-string-no-properties 3) "$'"))
               (t (concat "-run='" (match-string-no-properties 3) "$'")))))
    (message "Must be in a _test.go file to run go-run-test-current-function")))

(defun toogle-test-suite-off ()
  (interactive)
  (setq go-use-testify-for-testing nil)
)
(defun toogle-test-suite-on ()
  (interactive)
  (setq go-use-testify-for-testing t)
  )

(defun ms--project-root ()
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

(global-set-key (kbd "M-RET t r") 'projectile-run-project)

(defun spacemacs/go-run-package-tests-race ()
  (interactive)
  (spacemacs/go-run-tests "-race"))

(global-set-key (kbd "M-RET t R") 'spacemacs/go-run-package-tests-race)

