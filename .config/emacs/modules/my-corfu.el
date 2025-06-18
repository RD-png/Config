;;; my-corfu.el -*- lexical-binding: t; -*-

(use-package corfu
  :ensure (corfu :host github :repo "minad/corfu")
  :hook (lsp-completion-mode . corfu-setup-lsp)
  :bind (:map corfu-map
              ("<tab>" . corfu-insert)
              ("M-l" . corfu-show-location)
              ("M-SPC" . corfu-insert-separator))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-count 10
        corfu-preview-current nil
        corfu-auto-prefix 3
        corfu-auto-delay 0.01
        corfu-quit-at-boundary t
        corfu-echo-documentation nil
        corfu-quit-no-match t
        corfu-min-width 20
        corfu-max-width 60)
  (defun corfu-setup-lsp ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-popupinfo.el"))
  :after corfu
  :bind (:map corfu-map
              ("M-." . corfu-popupinfo-toggle)
              ([remap corfu-info-documentation] . corfu-popupinfo-toggle))
  :config
  (setq corfu-popupinfo-delay nil)
  :init (corfu-popupinfo-mode 1))

(use-package corfu-quick
  :ensure (:host github :repo "minad/corfu"
             :files ("extensions/corfu-quick.el"))
  :after corfu
  :bind (:map corfu-map
              ([remap avy-goto-char-timer] . corfu-quick-insert)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'my-corfu)
