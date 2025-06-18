;;; my-company.el -*- lexical-binding: t; -*-
;; Borrow dooms company config.

(defvar +lsp-company-backends '(:separate company-capf company-yasnippet))

(defvar +company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.
MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.
If the car of BACKENDS is nil, unset the backends for MODES.
Examples:
  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)
  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))
  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))
  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))

(defun +company--backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

(defun +company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
      (setq-local company-backends (+company--backends))))

(put '+company-init-backends-h 'permanent-local-hook t)

(use-package company
  :ensure t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :config
  (add-hook 'lsp-completion-mode-hook
            (defun +lsp-init-company-backends-h ()
              (when lsp-completion-mode
                (set (make-local-variable 'company-backends)
                     (cons +lsp-company-backends
                           (remove +lsp-company-backends
                                   (remq 'company-capf company-backends)))))))
  :init
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.01
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-auto-commit nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '(company-capf))
  (global-company-mode +1))

(add-hook 'after-change-major-mode-hook #'+company-init-backends-h)

(provide 'my-company)
