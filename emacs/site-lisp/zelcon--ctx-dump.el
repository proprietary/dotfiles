(defun zelcon/file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun zelcon/get-marked-files-from-all-dired-buffers ()
  "Get all marked files from all open dired buffers."
  (let (all-marked-files)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (let ((marked-files (dired-get-marked-files)))
            (setq all-marked-files (append all-marked-files marked-files))))))
    all-marked-files))

(defun zelcon/ctx-dump ()
  (interactive)
  (let* ((marked-files (zelcon/get-marked-files-from-all-dired-buffers))
         (new-buffer (generate-new-buffer "*Concatenated Files*")))
    (if marked-files
        (with-current-buffer new-buffer
          (dolist (file marked-files)
            (when (file-regular-p file)
              (let ((filename (file-relative-name file default-directory))
                    (file-contents (zelcon/file-contents file)))
                (insert (format "### File: %s\n%s\n\n" filename file-contents)))))
          (switch-to-buffer new-buffer))
      (progn
        (message "No files marked")
        (kill-buffer new-buffer)))))

(define-key dired-mode-map (kbd "C-c |") 'zelcon/ctx-dump)

(provide 'zelcon--ctx-dump)
