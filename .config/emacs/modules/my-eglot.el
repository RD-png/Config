;;; my-eglot.el -*- lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :commands eglot eglot-ensure
  :hook(eglot . +lsp-optimization-mode)
  :bind
  (:map eglot-mode-map
        ("C-c o R" . eglot-rename)
        ("C-c o f" . eglot-format)
        ("C-c o a" . eglot-code-actions)
        ("C-c o t" . eglot-find-typeDefinition)
        ("C-c o i" . eglot-find-implementation)
        ("C-c o h" . eglot-find-declaration)
        ("C-c o l" . consult-eglot-symbols))
  :preface
  (defun my/lsp-hook ()
    (eglot-ensure))
  :init
   (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-extend-to-xref t
        eglot-stay-out-of '(company)
        eglot-auto-display-help-buffer nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
   :config
   (fset #'jsonrpc--log-event #'ignore)
   (defadvice! +lsp--defer-server-shutdown-a (fn &optional server)
     "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
     :around #'eglot--managed-mode
     (letf! (defun eglot-shutdown (server)
              (if (or (null +lsp-defer-shutdown)
                      (eq +lsp-defer-shutdown 0))
                  (prog1 (funcall eglot-shutdown server)
                    (+lsp-optimization-mode -1))
                (run-at-time
                 (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
                 nil (lambda (server)
                       (unless (eglot--managed-buffers server)
                         (prog1 (funcall eglot-shutdown server)
                           (+lsp-optimization-mode -1))))
                 server)))
       (funcall fn server))))

(use-package xref
  :ensure nil
  :hook (xref-after-jump . recenter)
  :bind
  ("C-c o ," . xref-go-back)
  ("C-c o ." . xref-go-forward)
  ("C-c o r" . xref-find-references)
  ("C-c o s" . xref-find-apropos)
  ("C-c o g" . xref-find-definitions))

(use-package eldoc
  :ensure nil
  :bind
  ("C-c o d" . eldoc-doc-buffer)
  :init
  (global-eldoc-mode))

(use-package consult-eglot
  :ensure t
  :defer t)

(provide 'my-eglot)
