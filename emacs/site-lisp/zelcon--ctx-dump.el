(defun zelcon/file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun zelcon/ctx-dump ()
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (new-buffer (generate-new-buffer "*Concatenated Files*")))
    (if marked-files
        (with-current-buffer new-buffer
          (dolist (file marked-files)
            (let ((filename (file-relative-name file default-directory))
                  (file-contents (zelcon/file-contents file)))
              (insert (format "### File: %s\n%s\n\n" filename file-contents))))
          (switch-to-buffer new-buffer))
      (progn
        (message "No files marked")
        (kill-buffer new-buffer)))))

(define-key dired-mode-map (kbd "C-c |") 'zelcon/ctx-dump)

(provide 'zelcon--ctx-dump)
