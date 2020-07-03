dotspacemacs-configuration-layers
'(nginx
  python
  javascript
  yaml
  html
  ;; ----------------------------------------------------------------
  ;; Example of useful layers you may want to use right away.
  ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
  ;; `M-m f e R' (Emacs style) to install them.
  ;; ----------------------------------------------------------------
  ;; auto-completion
  ;; better-defaults
  emacs-lisp
  git
  helm
  lsp
  ;; markdown
  multiple-cursors
  org
  ;; (shell :variables
  ;;        shell-default-height 30
  ;;        shell-default-position 'bottom)
  spell-checking
  syntax-checking
  treemacs
  go
  ;; version-control
  ;; mu4e
  )

dotspacemacs-additional-packages '(
                                   go-dlv
                                   protobuf-mode
                                   go-imports
                                   gotest
                                   dockerfile-mode
                                   direnv
                                   yasnippet-snippets
                                   groovy-mode
                                   )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (load-file "/home/mshogin/go/src/github.com/mshogin/env-config/emacs/windows.el")
  (load-file "/home/mshogin/go/src/github.com/mshogin/env-config/emacs/key-bindings.el")
  (load-file "/home/mshogin/go/src/github.com/mshogin/env-config/emacs/go.el")
  (load-file "/home/mshogin/go/src/github.com/mshogin/env-config/emacs/general.el")
  (load-file "/home/mshogin/go/src/github.com/mshogin/env-config/emacs/ci.el")
  )
