;;; my-vertico.el -*- lexical-binding: t; -*-

(use-package vertico
  :ensure (vertico :repo "minad/vertico")
  :demand t
  :preface
  (defun consult-vertico-save ()
    "Cleaner version of vertico-save that works with
consult based prompts."
    (interactive)
    (embark--act #'kill-new (car (embark--targets)))
    (abort-minibuffers))
  :config
  (setq
   vertico-count 7
   vertico-cycle t
   vertico-resize nil)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :bind (:map vertico-map
              ("M-w" . consult-vertico-save)
              ("C-M-a" . vertico-first)
              ("C-M-e" . vertico-last))
  :init
  (vertico-mode))

(use-package vertico-directory
  :ensure (:host github :repo "minad/vertico"
             :files ("extensions/vertico-directory.el"))
  :after vertico
  :bind (:map vertico-map
              ("M-d" . vertico-directory-up)))

(provide 'my-vertico)
