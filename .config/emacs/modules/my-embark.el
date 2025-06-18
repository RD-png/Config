;;; my-embark.el -*- lexical-binding: t; -*-

(use-package embark
  :ensure t
  :demand t
  :after minibuffer
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :preface
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))
  :config
  (define-key embark-file-map (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  (define-key embark-file-map (kbd "S") 'sudo-find-file)
  :bind (:map minibuffer-local-map
              ("C-c C-o" . embark-export))
  :bind*
  ("C-o" . embark-act)
  ("C-h h" . embark-bindings))

(use-package embark-consult
  :ensure t
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'my-embark)
