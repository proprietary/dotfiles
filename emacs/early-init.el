;; initial Emacs frame size
(add-to-list 'initial-frame-alist '(width . 120))
(add-to-list 'initial-frame-alist '(height . 100))

(defun zelcon/make-path (&rest strings)
  (mapconcat #'identity strings ":"))

(when (eq system-type 'darwin)
  ;; Launched as a GUI from Finder, Emacs does not inherit shell env, so must be set here
  (setenv "LIBRARY_PATH"
	  (zelcon/make-path
	   (getenv "LIBRARY_PATH")
	   ;(concat (getenv "HOME") "/.local/lib")
	   "/usr/lib" "/usr/local/lib" "/opt/homebrew/lib" "/opt/homebrew/lib/gcc/current"
	   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/lib"))
  (setenv "LD_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
  (setenv "DYLD_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
  (setenv "PATH"
	  (zelcon/make-path
	   (getenv "PATH")
	   (concat (getenv "HOME") "/.local/bin")
	   "/usr/local/bin" "/opt/homebrew/bin"))
  (setenv "MANPATH"
	  (zelcon/make-path
	   (getenv "MANPATH")
	   (concat (getenv "HOME") "/.local/lib/share/man")
	   "/opt/homebrew/share/man" "/usr/share/man" "/usr/local/share/man"))
  (setenv "INFOPATH"
	  (zelcon/make-path
	   (getenv "INFOPATH")
	   (concat (getenv "HOME") "/.local/lib/share/info")
	   "/opt/homebrew/share/info")))
