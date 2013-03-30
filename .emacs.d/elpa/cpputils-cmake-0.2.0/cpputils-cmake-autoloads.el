;;; cpputils-cmake-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (cppcm-reload-all cppcm-compile cppcm-create-or-update-flymake-files
;;;;;;  cppcm-get-exe-path-current-buffer) "cpputils-cmake" "cpputils-cmake.el"
;;;;;;  (20823 23976 356003 877000))
;;; Generated autoloads from cpputils-cmake.el

(autoload 'cppcm-get-exe-path-current-buffer "cpputils-cmake" "\


\(fn)" t nil)

(autoload 'cppcm-create-or-update-flymake-files "cpputils-cmake" "\
Create flymake files used by flymake and data used by (cppcm-get-cppflags-in-current-buffer)

\(fn)" t nil)

(autoload 'cppcm-compile "cpputils-cmake" "\
compile the executable/library in current directory,
default compile command or compile in the build directory.
You can specify the sequence which compile is default
by customize `cppcm-compile-list'.

\(fn &optional PREFIX)" t nil)

(autoload 'cppcm-reload-all "cpputils-cmake" "\
re-create Makefiles for flymake and re-set all the flags

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("cpputils-cmake-pkg.el") (20823 23976
;;;;;;  479056 239000))

;;;***

(provide 'cpputils-cmake-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cpputils-cmake-autoloads.el ends here
