;;; my-completion.el -*- lexical-binding: t; -*-

(use-package prescient
  :ensure t
  :custom
  (prescient-history-length 1000)
  :init
  (setq prescient-persist-mode t))

(use-package affe
  :ensure t
  :defer 2
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight)
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode +1))

(provide 'my-completion)
