;;; my-consult.el -*- lexical-binding: t; -*-

(use-package consult
  :ensure t
  :after project
  :demand t
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-project-root-function (lambda () "Return current project root"
                                        (project-root (project-current))))
  (setq consult-narrow-key "<")
  (setq consult-preview-key "M-.")
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-line-start-from-top nil)
  (consult-line-point-placement 'match-end)
  (fset 'multi-occur #'consult-multi-occur)
  :bind (([remap yank-pop] . consult-yank-pop)
         ([remap apropos] . consult-apropos)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap locate] . consult-locate)
         ([remap load-theme] . consult-theme)
         ([remap man] . consult-man)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap list-buffers] . consult-buffer)
         ("C-c o l" . consult-imenu)
         ("C-c o L" . consult-imenu-multi)
         ("C-M-s" . consult-multi-occur)
         ("C-M-l" . consult-outline)
         ("M-g M-g" . goto-line)
         ("C-c f" . consult-flymake)
         ("C-x M-f" . recentf-open-files)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :bind*
  ("C-s" . consult-line)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  )

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-map
         ("C-x j" . consult-dir-jump-file)))

(provide 'my-consult)
