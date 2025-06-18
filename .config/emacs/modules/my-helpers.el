;;; my-helpers.el -*- lexical-binding: t; -*-

;;;###autoload
(defun sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (sudo-file-path file)))

;;;###autoload
(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;###autoload
(defun sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

(provide 'my-helpers)
