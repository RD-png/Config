;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; GC alternative.
;; (add-hook 'emacs-startup-hook
;;           (defun emacs-startup-reset-defaults+ ()
;;             (setq default-gc-cons-threshold 16777216
;;                   gc-cons-percentage 0.1)))

;; (defun my/defer-garbage-collection ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my/restore-garbage-collection ()
;;   (run-at-time 1 nil (lambda () (setq gc-cons-threshold 16777216))))

;; (add-hook 'minibuffer-setup-hook 'my/defer-garbage-collection)
;; (add-hook 'minibuffer-exit-hook 'my/restore-garbage-collection)


;;; Native Comp.
(custom-set-variables
 '(warning-suppress-log-types '((org-element-cache))))
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq warning-minimum-level :error)
    (setq package-native-compile t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))))


;; Elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; Core Modules.
(require 'my-doom)
(require 'core)
(require 'ui)
;; (require 'my-tree-sitter)
(require 'my-vertico)
(require 'my-corfu)
;; (require 'my-company)
(require 'my-orderless)
(require 'my-completion)
(require 'my-consult)
(require 'my-embark)
(require 'my-navigation)
(require 'my-lsp)
(require 'my-lsp-mode)
;; (require 'my-eglot)
(require 'my-langs)
(require 'my-workspaces)
;; (require 'my-perspective)
(require 'my-workflow)
(require 'my-editing)
(require 'my-org)
;; (require 'my-eshell)
(require 'my-vterm)
(require 'my-helpers)
(require 'my-theme)

(elpaca-wait)

;;; init.el ends here.
