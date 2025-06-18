;;; my-vterm.el -*- lexical-binding: t; -*-

(use-package vterm
  :ensure t
  :commands vterm-mode
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (add-hook 'vterm-mode-hook 'set-no-process-query-on-exit))

(provide 'my-vterm)
