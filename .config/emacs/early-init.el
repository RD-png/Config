;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(setq inhibit-x-resources t)
(setq x-gtk-use-system-tooltips t)

(setq package-enable-at-startup nil
      package-quickstart nil
      load-prefer-newer t)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)

    (defun reset-file-handler ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))

    (add-hook 'emacs-startup-hook #'reset-file-handler 101))

  (setq-default inhibit-redisplay t
                inhibit-message t)

  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

  (define-advice startup--load-user-init-file (:before (&rest _) nomessage-remove)
    (advice-remove #'load-file #'load-file@silence)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(setq frame-inhibit-implied-resize t)

(setenv "LSP_USE_PLISTS" "true")
(set-language-environment "UTF-8")
