;;; my-eshell.el -*- lexical-binding: t; -*-

(defun my/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun my/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun my/pwd-shorten-dirs (pwd)
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 0)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))

(defun my/eshell-prompt ()
  (concat
   "\n"
   (propertize (user-login-name) 'face `(:foreground "#8f0075"))
   (propertize " @ " 'face `(:foreground "#2544bb"))
   (propertize (my/pwd-shorten-dirs (my/get-prompt-path)) 'face `(:foreground "#145c33"))
   (propertize " #" 'face `(:foreground "#70480f"))
   (propertize " " 'face `(:foreground "white"))))

(defun eshell-configure ()
  (use-package xterm-color
    :ensure t)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  (define-key eshell-mode-map (kbd "<tab>") 'capf-autosuggest-forward-word)
  (define-key eshell-mode-map (kbd "C-r") 'consult-history)
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-l") (lambda () (interactive) (eshell/clear 1) (eshell-send-input)))
  (eshell-hist-initialize)
  (setenv "PAGER" "cat")

  (setq eshell-prompt-function 'my/eshell-prompt
        eshell-prompt-regexp "[a-zA-z]+ @ [^#$\n]+ # "
        eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil
        comint-prompt-read-only t)
  (generate-new-buffer eshell-buffer-name))

(use-package eshell
  :ensure nil
  :defer t
  :hook (eshell-first-time-mode . eshell-configure)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh"))))

(use-package eshell-syntax-highlighting
  :ensure t
  :defer t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package capf-autosuggest
  :ensure (capf-autosuggest :host github :repo "emacs-straight/capf-autosuggest")
  :disabled t
  :hook ((eshell-mode comint-mode) . capf-autosuggest-mode))

(provide 'my-eshell)
