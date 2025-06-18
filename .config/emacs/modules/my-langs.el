;;; my-langs.el -*- lexical-binding: t; -*-

(setq font-lock-maximum-decoration '((t . t)))

;;;###autoload
(defun mode-unicode-conversions (modes unicode-chars)
  (mapc (lambda (mode)
            (font-lock-add-keywords
             mode
             (append (mapcar (lambda (chars)
                               `(,(car chars)
                                 ,(if (characterp (cdr chars))
                                      `(0 (ignore
                                           (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ,(cdr chars))))
                                    `(0 ,(cdr chars)))))
                             unicode-chars))))
        modes))


(defun set-font-decoration (mode level)
  (setq font-lock-maximum-decoration (cons
                                      (cons mode level)
                                      font-lock-maximum-decoration)))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :hook (prog-mode . display-fill-column-indicator-mode)
  :hook (before-save . delete-trailing-whitespace)
  :bind (:map prog-mode-map
              ("C-a" . my/beginning-of-line)))

(use-package conf-mode
  :ensure nil
  :hook (conf-mode . display-line-numbers-mode))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :hook (php-mode . my/lsp-hook))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . display-line-numbers-mode))

(use-package ts
  :ensure t)

(use-package htmlize
  :ensure t
  :config
  (setq org-html-htmlize-output-type 'css))

(use-package pip-requirements
  :ensure t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package python-mode
  :ensure t
  :hook (python-mode . my/lsp-hook)
  :bind (:map python-mode-map
              ([remap lsp-format-buffer] . python-black-buffer))
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook
            (lambda()
              (local-unset-key (kbd "DEL")))))

(use-package pyimport
  :ensure t
  :after python-mode)

(use-package pyvenv
  :ensure t
  :defer 5
  :config
  (setq pyvenv-menu t))

(use-package python-black
  :ensure t
  :defer 5)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . my/lsp-hook))

(use-package nix-update
  :ensure t
  :commands nix-update-fetch)

(use-package web-mode
  :ensure t
  :mode ("\\.vue\\'")
  :hook (web-mode . my/lsp-hook)
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1
        web-mode-indent-style 2
        web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing t)
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal) "//"))

(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'"))

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'")
  :hook (haskell-mode . my/lsp-hook)
  :hook (haskell-mode . dumb-jump-mode)
  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-compile)
              ("C-c m s" . run-haskell)
              ([remap my/lsp-format-buffer] . haskell-mode-stylish-buffer))
  :config
   (setq haskell-process-suggest-remove-import-lines t
         haskell-process-auto-import-loaded-modules t
         haskell-process-type 'cabal-repl)
  :preface
  (defconst haskell-unicode-conversions
  '(("[ (]\\(\\.\\)[) ]"       . ?∘)
    ("[ (]\\(\\\\\\)[(_a-z]"   . ?λ)
    ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)))
  :init
  (mode-unicode-conversions '(haskell-mode haskell-literate-mode) haskell-unicode-conversions))

(use-package lsp-haskell
  :ensure t
  ;; :config
  ;; (setq lsp-haskell-plugin-ghcide-type-lenses-global-on nil)
  ;; (setq lsp-haskell-plugin-import-lens-code-lens-on nil)
  ;; (setq lsp-haskell-plugin-import-lens-code-actions-on nil)
  ;; (setq lsp-haskell-plugin-ghcide-type-lenses-config-mode nil)
  )

(use-package typescript-mode
  :ensure t
  :mode
  ("\\.ts\\'"
   "\\.js\\'")
  :hook (typescript-mode . my/lsp-hook)
  :config
  (setq typescript-indent-level 4))

(use-package emacs-lisp-mode
  :ensure nil
  :hook (lisp-mode . emacs-lisp-mode)
  :hook (emacs-lisp-mode . paredit-mode)
  :bind ((:map emacs-lisp-mode-map
               ("C-c m s" . ielm)))
  :preface
  (defconst elisp-unicode-conversions
  '(("[ (]\\(\\lambda\\)[) ]"       . ?λ)))
  :init
  (mode-unicode-conversions '(emacs-lisp-mode) elisp-unicode-conversions))

(use-package scheme-mode
  :ensure nil
  :mode ("\\.sld\\'"))

(use-package geiser
  :ensure t
  :config
  (setq geiser-scheme-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :ensure t
  :after geiser)


(use-package racket-mode
  :ensure t
  :mode ("\\.rkt\\'"))

(use-package clojure-mode
  :ensure t
  :mode ("\\.clj\\'")
  :hook (clojure-mode . my/lsp-hook)
  :hook (clojure-mode . paredit-mode)
  :bind ((:map clojure-mode-map
              ("C-c m s" . cider-jack-in-clj)))
  :config
  (setq-local lsp-lens-enable t))

(use-package cider
  :ensure t
  :bind ((:map cider-mode-map
               ("C-x C-e" . cider-eval-last-sexp)
               ("C-c C-e" . cider-eval-buffer)
               ("C-c e r" . cider-eval-region))
         :map cider-repl-mode-map
         ("S-<return>" . cider-repl-newline-and-indent)
         ("C-l" . cider-repl-clear-buffer)))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'")
  :hook(go-mode . my/lsp-hook)
  :config
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (unusedwrite . t)
          (unusedparams . t)))
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t)
       ("gopls.experimentalWorkspaceModule" t t))))
  (setq lsp-gopls-server-path "/home/ryan/go/bin/gopls"))

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :hook (rustic-mode-hook . rustic-lsp-mode-setup)
  :config
  (setq rustic-lsp-server 'rls)
  (setq rustic-lsp-server 'rustfmt)
  (setq rustic-lsp-client 'lsp-mode)
  (setq rustic-indent-method-chain t))

(use-package erlang
  :ensure t
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :hook (erlang-mode . my/lsp-hook)
  :hook (erlang-mode . erlang-edoc-mode)
  :bind ((:map erlang-mode-map
              ("C-c m s" . erlang-shell-rebar)
              ("C-c C-c" . recompile)
              ([remap consult-eglot-symbols] . consult-imenu)
              ([remap erlang-electric-newline] . newline-and-indent))
         :map erlang-shell-mode-map
         ("C-c C-g" . comint-interrupt-subjob)
         ("C-c C-c" . erlang-shell-rebar-reload)
         ("C-c m s" . erlang-shell-rebar))
  :config
  (set-font-decoration 'erlang-mode 3)
  (defun erlang-shell-rebar ()
    (interactive)
    (inferior-erlang "make dev"))
  (defun erlang-shell-rebar-reload ()
    (interactive)
    (comint-interrupt-subjob)
    (kill-buffer)
    (erlang-shell-rebar))
  (setq erlang-indent-level 2)
  (setq erlang-indent-guard 2)
  (setq erlang-icr-indent 2)
  (setq erlang-electric-commands nil)
  (setq inferior-erlang-machine-options '("-sname" "emacs")))

(use-package tuareg
  :ensure t
  :mode ("\\.ml$" . tuareg-mode)
  :bind ((:map tuareg-mode-map
               ("C-c m s" . utop)))
  :custom-face
  (tuareg-font-lock-governing-face ((t (:foreground unspecified :bold unspecified))))
  ;; :hook (tuareg-mode . merlin-mode)
  :hook (tuareg-mode . lsp-lens-mode)
  :hook (tuareg-mode . my/lsp-hook))

(use-package merlin
  :ensure t
  :disabled t
  :bind (:map merlin-mode-map
              ("C-c o g" . merlin-locate)
              ("C-c o l" . merlin-use-merlin-imenu)
              ("C-c o r" . merlin-occurrences)))

(use-package utop
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml$" . yaml-mode))

(use-package latex
  :ensure nil
  :defer 5
  :after tex
  :mode ("\\.tex\\'" . LaTeX-mode))

;; (use-package auctex
;;   :ensure nil)

(use-package cdlatex
  :ensure nil
  :defer 5
  :after latex
  :hook (LaTeX-mode . turn-on-cdlatex))

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package devdocs
  :ensure t
  :defer 2
  :config
  (defun my/devdocs-lookup ()
    (interactive)
    (devdocs-lookup nil (thing-at-point 'word 'no-properties)))
  (add-hook 'web-mode-hook
            (lambda () (setq-local devdocs-current-docs '("vue~3"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("django_rest_framework" "django~3.2"))))
  (add-hook 'php-mode-hook
            (lambda () (setq-local devdocs-current-docs '("laravel~8"))))
  :bind ("C-c o D" . my/devdocs-lookup))

(provide 'my-langs)
