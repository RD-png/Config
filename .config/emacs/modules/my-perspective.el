;;; my-perspective.el -*- lexical-binding: t; -*-

(use-package perspective
  :ensure t
  :hook (after-init . (lambda () (persp-switch "Main") (persp-add-buffer "*Messages*")))
  :bind (("C-c s s" . persp-switch)
         ("C-c s k" . persp-kill)
         ("C-c s n" . persp-next)
         ("C-c s p" . persp-prev)
         ("C-c s r" . persp-rename)
         ("C-c s b" . persp-switch-to-buffer)
         ("C-x C-'" . persp-switch-last))
  :custom
  (persp-initial-frame-name "Alt")
  (persp-show-modestring t)
  (persp-modestring-short t)
  (persp-modestring-dividers '("<" ">" ""))
  (doom-modeline-persp-name t)
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))

(provide 'my-perspective)
