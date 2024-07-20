;;; format-sql.el ---- Reformat SQL scripts using sleek -*- lexical-binding: t; -*-

;; Copyright (C) 2024   Zelly Snyder

;; Author: Zelly Snyder <zelcon@zelcon.net>
;; Keywords: languages
;; URL: https://github.com/proprietary/dotfiles
;; Package-Requires: ((emacs "29" (reformatter "0.3")))
;; Version: 1

;;; Commentary

;; Formats SQL in a region or in a buffer.
;;
;; Requires sleek(1), which you can install with:
;; $ cargo install sleek
;;

(require 'reformatter)

(defgroup format-sql nil "Reformat SQL scripts" :group 'languages)

(defcustom format-sql-command "sleek" "Command used for formatting SQL scripts" :type 'string)

;;;###autoload (autoload 'format-sql-buffer "zelcon/format-sql" nil t)
(reformatter-define format-sql
  :program format-sql-command
  :args nil
  :lighter " SQLFormat"
  :group 'format-sql)

(provide 'format-sql)
