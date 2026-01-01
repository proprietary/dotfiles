(defun zelcon/file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun zelcon/get-marked-files-from-all-dired-buffers ()
  "Get all marked files from all open dired buffers."
  (let ((all-marked-files '()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'dired-mode)
          (let ((marked-files (dired-get-marked-files nil nil nil t)))
            (cond
             ((eq (car marked-files) t)
              (setq all-marked-files (append all-marked-files (cdr marked-files)))
              (dired-unmark-all-files ?\r))
             ((> (length marked-files) 1)
              (setq all-marked-files (append all-marked-files marked-files))
              (dired-unmark-all-files ?\r))
             ;; no marks ⟹ this is a dired buffer with no actual marked files
             ;; dired-get-marked-files picked up a file under the cursor, which should be ignored
             (t nil))))))
      all-marked-files))

(defun zelcon/ctx-dump ()
  "Concatenates all files marked in dired buffers"
  (interactive)
  (let ((marked-files (zelcon/get-marked-files-from-all-dired-buffers))
        (new-buffer (generate-new-buffer "*Concatenated Files*")))
    ;; fallback: if no files were marked, use the file under the current buffer's cursor
    (unless marked-files
      (when (derived-mode-p 'dired-mode)
        (setq marked-files (dired-get-marked-files))))
    (if marked-files
        (with-current-buffer new-buffer
          (insert "I am providing the following files for context:\n\n")
          (dolist (file marked-files)
            (when (file-regular-p file)
              (let ((filename (file-relative-name file default-directory))
                    (file-contents (zelcon/file-contents file)))
                ;; Format: <file path="filename"> content </file>
                (insert (format "<file path=\"%s\">\n%s\n</file>\n\n"
                                filename
                                file-contents)))))
          (switch-to-buffer new-buffer))
      (progn
        (message "No files marked")
        (kill-buffer new-buffer)))))

(define-key dired-mode-map (kbd "C-c |") 'zelcon/ctx-dump)

(provide 'zelcon--ctx-dump)
